module Env = Map.Make(String)
external ident : 'a -> 'a = "%identity"
exception VariableNotFound of string
exception SyntaxError of string
exception Unapplicable
exception TypeError

let sprintf = Printf.sprintf
let dEBUG = ref false

type exp =
| LInt of int
| LBool of bool
| LString of string
| LFloat of float
| Var of string
| LSexp of exp list
type value =
| VInt of int
| VBool of bool
| VString of string
| VFloat of float
| VSymbol of string
| VCons of value * value
| VNil
| VUnit
| VProcedure of proc
| VPrimitive of prim
and ret = value
and env = {
  frame: ret Env.t;
  enclosing: env ref option
}
and proc = {
  env: env ref;
  params: string list;
  body: exp list
}
and prim = ret list -> ret

let string_of_list ?(separator=" ") f ls =
  let rec f0 a = match a with
  | [] -> "" | [h] -> (f h)
  | h::t -> (f h) ^ separator ^ (f0 t)
  in f0 ls
let rec string_of_exp = function
| LInt n -> string_of_int n
| LBool b -> string_of_bool b
| LString s -> s
| LFloat f -> string_of_float f
| Var  x -> x
| LSexp ls -> "[" ^ (string_of_list string_of_exp ls) ^ "]"
let rec string_of_value = function
| VInt n -> string_of_int n
| VBool b -> string_of_bool b
| VString s -> s
| VFloat f -> string_of_float f
| VSymbol s -> "`" ^ s
| VCons (u,v) ->
  sprintf "(%s,%s)"
    (string_of_value u) (string_of_value v)
| VNil -> "`()"
| VUnit -> "()"
| VProcedure {env; params; body} ->
  sprintf "λ:[%s]->[%s]"
    (string_of_list ident params) (string_of_list string_of_exp body)
| VPrimitive _ -> "<primitive>"
and string_of_ret x = string_of_value x
and string_of_env ?(ln=false) env =
  let max_depth = 1 in
  let rec lp {frame; enclosing} depth =
    if depth > max_depth then "..."
    else
      let str_frame = Env.fold (fun k v str ->
        (if str = "" then "" else str ^ ";" ^
          (if ln then "\n  " else ""))
        ^ k ^ "=" ^ string_of_ret v
      ) frame "" in
      sprintf "{frame={%s}; enclosing=%s}" str_frame
        @@ match enclosing with
        | None -> "{}"
        | Some env' -> lp !env' (depth+1)
  in lp env 0


let new_env base = {frame=Env.empty; enclosing=Some base}

let lookup_variable var env =
  let rec lp = function
  | None -> raise @@ VariableNotFound var
  | Some env ->
    match Env.find_opt var !env.frame with
    | Some x -> x
    | None -> lp !env.enclosing
  in lp (Some env)

let rec eval exp env = match exp with
| LInt n -> VInt n
| LBool b -> VBool b
| LString s -> VString s
| LFloat f -> VFloat f
| Var x -> lookup_variable x env
| LSexp ls -> match ls with
  | [] -> raise @@ SyntaxError "lsexp should not be empty"
  | (Var "define")::(Var var)::body ->
    let value = eval_many body env in
    env := {!env with frame=Env.add var value !env.frame};
    VUnit
  | (Var "lambda")::(LSexp param_ls)::body ->
    let params = List.map (function
    | Var x -> x
    | _ -> raise @@ SyntaxError "not a variable") param_ls
    in VProcedure {env; params; body}
  | [Var "quote"; body] -> (
    let rec eval_quote body =
      match body with
    | LInt _ | LBool _ | LString _ | LFloat _ -> eval body env
    | Var x -> VSymbol x
    | LSexp ls ->
      let rec lp = function
      | h::t -> VCons (eval_quote h, lp t)
      | [] -> VNil
      in lp ls
    in eval_quote body)
  | [Var "if";cond;consq;alt] -> (
    match eval cond env with
    | VBool true -> eval consq env
    | VBool false -> eval alt env
    | _ -> raise TypeError)
  | (Var "list")::body -> (
    let body = List.map (fun exp -> eval exp env) body in
    let rec loop ls = function
    | [] -> ls
    | h::t -> loop (VCons (h,ls)) t in
    loop VNil @@ List.rev body)
  | (Var "time_internal")::body ->
    let tic = Sys.time () in
    let _ = eval_many body env in
    let toc = Sys.time () in
    VFloat (toc -. tic)
  | hd::_ ->
    let head = eval hd env in
    match head with
    | VProcedure _ | VPrimitive _ ->
      let args = List.tl ls |> List.map (fun a -> eval a env) in
      apply head args
    | _ -> raise @@ SyntaxError (
      sprintf "not implemented or invalid sexp: %s"
      @@ string_of_exp exp
    )
and eval_many exps env =
  let rec lp = function
  | [] -> raise @@ SyntaxError "empty"
  | [exp] -> eval exp env
  | exp::tl ->
    assert (VUnit = eval exp env); lp tl
  in lp exps
and apply proc args = match proc with
| VProcedure {env; params; body} ->
  let env' =
    let e0 = new_env env in
    ref {e0 with frame=
      List.fold_left2 (fun frame p a ->
        Env.add p a frame
      ) e0.frame params args
    } in
  eval_many body env'
| VPrimitive func -> func args
| _ -> raise Unapplicable

let interpret ast env =
  List.iter (fun exp ->
    Printf.printf "input : %s\n%!" @@ string_of_exp exp;
    let ret = eval exp env in
    Printf.printf "-->     %s\n%!" @@ string_of_ret ret;
    if !dEBUG then
      Printf.printf "  (env: %s)\n%!"@@ string_of_env !env;
    Printf.printf "\n%!";
  ) ast;
  Printf.printf "============\n%!"

let vprim_of name pat = name, match pat with
| `I_I_I f -> (function [VInt u; VInt v] -> VInt (f u v)
  | _ -> raise TypeError)
| `B_B_B f -> (function [VBool u; VBool v] -> VBool (f u v)
  | _ -> raise TypeError)
| `I_I_B f -> (function [VInt u; VInt v] -> VBool (f u v)
  | _ -> raise TypeError)
| `Any_U f -> (function [v] -> f v
  | _ -> raise TypeError)
| `B_B f -> (function [VBool v] -> VBool (f v)
  | _ -> raise TypeError)
| `B_U f -> (function [VBool v] -> f v; VUnit
  | _ -> raise TypeError)
| `A_A_B f -> (function [p; q] -> VBool (f p q)
  | _ -> raise TypeError)
| `n_U f -> (function [] -> f (); VUnit
  | _ -> raise TypeError)
| `L_A f -> (function [(VCons _) as p] | [VNil as p] -> f p
  | _ -> raise TypeError)
| `L_L f -> (function [(VCons _) as p] | [VNil as p] -> f p
  | _ -> raise TypeError)
| `L_L_L f -> (function [p;q] -> (match p,q with
    | VCons _,VCons _ | VCons _,VNil | VNil,VCons _ | VNil,VNil -> f p q
    | _ -> raise TypeError)
  | _ -> raise TypeError)
| `S_S_S f -> (function [VString p;VString q] -> VString (f p q)
  | _ -> raise TypeError)
| `I_S f -> (function [VInt p] -> VString (f p)
  | _ -> raise TypeError)
| `n_I f -> (function [] -> VInt (f ())
  | _ -> raise TypeError)
| `n_S f -> (function [] -> VString (f ())
  | _ -> raise TypeError)
| `F_F_F f -> (function [VFloat u; VFloat v] -> VFloat (f u v)
  | _ -> raise TypeError)
| `I_F f -> (function [VInt v] -> VFloat (f v)
  | _ -> raise TypeError)
| `F_I f -> (function [VFloat v] -> VInt (f v)
  | _ -> raise TypeError)
| `F_F f -> (function [VFloat v] -> VFloat (f v)
  | _ -> raise TypeError)
| `I_U f -> (function [VInt v] -> (f v; VUnit)
  | _ -> raise TypeError)
| `A_S f -> (function [v] -> (VString (f v))
  | _ -> raise TypeError)
| `A_A f -> (function [v] -> f v | _ -> raise TypeError)
| `n_F f -> (function [] -> VFloat (f ()) | _ -> raise TypeError)
| `S_I f -> (function [VString s] -> VInt (f s) | _ -> raise TypeError)
| `A_L_L f -> (function [u; VCons _ as v] -> f u v
  | [u; VNil] -> f u VNil
  | _ -> raise TypeError)
let add_vprims_to_frame frame prim_fun =
  List.map (fun (name,value) ->
    vprim_of name value
  ) prim_fun
  |> List.fold_left (fun frm (name,prm) ->
    Env.add name (VPrimitive prm) frm
  ) frame

let setup_env () =
  let env = {frame=Env.empty; enclosing=None} in
  let rev_list ls =
    let rec lp u v = match u,v with
    | VNil,ret -> ret
    | VCons (p,q),ret -> lp q (VCons (p,ret))
    | _ -> raise TypeError in
    lp ls VNil in
  let prim_var = [
    "true",  VBool true;
    "false", VBool false;
    "the_answer_to_everything", VInt 42;
    "pi", VFloat (2. *. acos 0.);
    "explode", VPrimitive (function
      | [VString s] ->
        let ls = List.init (String.length s) (fun i -> s.[i]) in
        let str = String.make 1 in
        let rec lp acc = function
        | [] -> rev_list acc
        | h::t -> lp (VCons (VString (str h),acc)) t
        in lp VNil ls
      | _ -> raise TypeError);
    "print", VPrimitive (fun ls ->
      print_endline @@ String.concat " " @@ List.map string_of_ret ls;
      VUnit)
  ] in
  let prim_fun = [
    "+",    `I_I_I (+);
    "-",    `I_I_I (-);
    "*",    `I_I_I ( * );
    "/",    `I_I_I (/);
    "mod",  `I_I_I (mod);
    "pow",  `I_I_I (fun u v ->
      let rec pow a n = function
      | 0 -> a | 1 -> n * a
      | k when k mod 2 = 0 -> pow a (n*n) (k/2)
      | k -> pow (a*n) (n*n) (k/2)
      in pow 1 u v
    );
    "<",    `A_A_B (<);
    ">",    `A_A_B (>);
    "^",    `S_S_S (^);
    "<=",   `A_A_B (<=);
    ">=",   `A_A_B (>=);
    "string_of_int",  `I_S (string_of_int);
    "and",  `B_B_B (&&);
    "or",   `B_B_B (||);
    "not",  `B_B (not);
    "debug",`B_U (fun b -> dEBUG := b);
    "=",    `A_A_B (=);
    "car",  `L_A (function
    | VCons (u,v) -> u
    | _ -> raise TypeError);
    "cdr",  `L_L (function
    | VCons (u,v) -> v
    | _ -> raise TypeError);
    "cons", `A_L_L (fun u v -> VCons (u,v));
    "rev",  `L_L (fun v -> rev_list v);
    "append", `L_L_L (fun u v ->
      let rec lp acc u v = match u, v with
      | VCons (p,q), r -> lp (VCons (p,acc)) q r
      | VNil, VCons (p,q) -> lp (VCons (p, acc)) VNil q
      | VNil, VNil -> rev_list acc
      | _ -> raise TypeError in lp VNil u v);
    "read_int",   `n_I (fun () -> read_int ());
    "read_line",  `n_S (fun () -> read_line ());
    "read_string",`n_S (fun () -> Scanf.scanf " %s" ident);
    "+.", `F_F_F (+.);
    "*.", `F_F_F ( *. );
    "-.", `F_F_F (-.);
    "/.", `F_F_F (/.);
    "fpow", `F_F_F ( ** );
    "int_of_float", `F_I (int_of_float);
    "float_of_int", `I_F (float_of_int);
    "random_init", `I_U (Random.init);
    "random_float", `F_F (Random.float);
    "show_type", `A_S (function
    | VInt _ -> "int"
    | VBool _ -> "bool"
    | VString _ -> "string"
    | VFloat _  -> "float"
    | VNil -> "list(empty)"
    | VCons _ -> "list"
    | VUnit -> "unit"
    | VSymbol _ -> "symbol"
    | VPrimitive _ -> "prim"
    | VProcedure _ -> "fun");
    "time_now", `n_F (Sys.time);
    "string_length", `S_I (String.length)
  ] in
  let frame = env.frame in
  let frame = List.fold_left (fun frm (var,value) ->
    Env.add var value frm
  ) frame prim_var in
  let frame = add_vprims_to_frame frame prim_fun
  in
  {env with frame}

let rec add_more_prims_env env_global =
  let {frame;_} as env = !env_global in
  let prim_fun = [
    "show_env", `n_U (fun () ->
      print_endline
      @@ string_of_env ~ln:true !env_global);
    "reset_env", `n_U (fun () ->
      env_global := setup_env ();
      add_more_prims_env env_global);
    "about",`n_U (fun () ->
      print_endline "This is a lisp-like language interpreter written in OCaml.");
    "help", `n_U (fun () ->
      print_endline "I'm afraid there is no help yet.");
    "exit", `n_U (fun () -> raise Exit);
  ] in
  env_global := {env with frame=add_vprims_to_frame frame prim_fun}
