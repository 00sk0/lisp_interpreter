module Env = Map.Make(String)
external ident : 'a -> 'a = "%identity"
exception VariableNotFound of string
exception SyntaxError of string
exception Unapplicable
exception TypeError

let dEBUG = ref false

type exp =
| LInt of int
| LBool of bool
| LString of string
| Var of string
| LSexp of exp list
type value =
| VInt of int
| VBool of bool
| VString of string
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
and prim = {
  name: string;
  arity: int;
  func : ret list -> ret
}

let string_of_list ?(separator=";") f ls =
  let rec f0 a = match a with
  | [] -> "" | [h] -> (f h)
  | h::t -> (f h) ^ separator ^ (f0 t)
  in f0 ls
let rec string_of_exp = function
| LInt n -> string_of_int n
| LBool b -> string_of_bool b
| LString s -> s
| Var  x -> x
| LSexp ls -> "[" ^ (string_of_list string_of_exp ls) ^ "]"
let rec string_of_value = function
| VInt n -> string_of_int n
| VBool b -> string_of_bool b
| VString s -> s
| VCons (u,v) ->
  Printf.sprintf "(%s,%s)"
    (string_of_value u) (string_of_value v)
| VNil -> "`()"
| VUnit -> "()"
| VProcedure {env; params; body} ->
  Printf.sprintf "λ:[%s]->[%s]"
    (string_of_list ident params) (string_of_list string_of_exp body)
| VPrimitive {name; arity; _} ->
  Printf.sprintf "%s#%d" name arity
and string_of_ret x = string_of_value x
and string_of_env env =
  let max_depth = 1 in
  let rec lp {frame; enclosing} depth =
    if depth > max_depth then "..."
    else
      let str_frame = Env.fold (fun k v str ->
        (if str = "" then "" else str ^ ";")
        ^ k ^ "=" ^ string_of_ret v
      ) frame "" in
      Printf.sprintf "{frame={%s}; enclosing=%s}" str_frame
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
  | hd::_ ->
    let head = eval hd env in
    match head with
    | VProcedure _ | VPrimitive _ ->
      let args = List.tl ls |> List.map (fun a -> eval a env) in
      apply head args
    | _ -> raise @@ SyntaxError (
      Printf.sprintf "not implemented or invalid sexp: %s"
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
      let prod = List.map2 (fun u v -> u,v) params args in
      List.fold_left (fun frame (p,a) ->
        Env.add p a frame
      ) e0.frame prod
    } in
  eval_many body env'
| VPrimitive {arity; func; _} ->
  assert (arity = List.length args);
  func args
| _ -> raise Unapplicable

let interpret ast env =
  let open Printf in
  printf "============\n%!";
  List.iter (fun exp ->
    printf "input : %s\n%!" @@ string_of_exp exp;
    let ret = eval exp env in
    printf "-->     %s\n%!" @@ string_of_ret ret;
    if !dEBUG then printf "  (env: %s)\n%!"@@ string_of_env !env;
  ) ast

let setup_env () =
  let env = {frame=Env.empty; enclosing=None} in
  let prim_var = [
    "true",  VBool true;
    "false", VBool false;
    "the_answer_to_everything", VInt 42
  ] in
  let prim_fun = [
    "+",   `I_I_I (+);
    "-",   `I_I_I (-);
    "*",   `I_I_I ( * );
    "/",   `I_I_I (/);
    "pow", `I_I_I (fun u v ->
      let rec pow a n = function
      | 0 -> a | 1 -> n * a
      | k when k mod 2 = 0 -> pow a (n*n) (k/2)
      | k -> pow (a*n) (n*n) (k/2)
      in pow 1 u v
    );
    "and",  `B_B_B (&&);
    "or",   `B_B_B (||);
    "not",  `B_B (not);
    "print",`Any_U (fun rets ->
      print_endline @@ string_of_list string_of_ret rets;
      VUnit);
    "debug", `B_U (fun b -> dEBUG := b)
  ] in
  let frame = env.frame in
  let frame = List.fold_left (fun frm (var,value) ->
    Env.add var value frm
  ) frame prim_var in
  let frame =
    List.map (fun (name,value) -> match value with
    | `I_I_I f -> {
      name; arity=2;
      func=function [VInt u; VInt v] -> VInt (f u v)
      | _ -> raise TypeError }
    | `B_B_B f -> {
      name; arity=2;
      func=function [VBool u; VBool v] -> VBool (f u v)
      | _ -> raise TypeError }
    | `Any_U f -> { name; arity=1; func=f }
    | `B_B f -> {
      name; arity=1;
      func=function [VBool v] -> VBool (f v)
      | _ -> raise TypeError }
    | `B_U f -> {
      name; arity=1;
      func=function [VBool v] -> f v; VUnit
      | _ -> raise TypeError }
    ) prim_fun
    |> List.fold_left (fun frm prm ->
      Env.add prm.name (VPrimitive prm) frm
    ) frame
  in
  ref {env with frame}

let test () =
  interpret [
    LInt 42;
    LInt 2434;
  ] (ref {frame=Env.empty; enclosing=None});
  interpret [
    Var "x"
  ] (ref {frame=Env.singleton "x" @@ VInt 2434; enclosing=None});
  interpret [
    Var "y"
  ] (ref {
    frame=Env.singleton "x" @@ VInt 2434; enclosing=Some (ref {
      frame=Env.singleton "y" @@ VInt 42; enclosing=None
    }
  )});
  interpret [
    Var "x";
    LSexp [Var "define"; Var "x"; LInt 42];
    Var "x"
  ] (ref {
    frame=Env.singleton "x" @@ VInt 2434; enclosing=None
  });
  interpret [
    LSexp [Var "lambda"; LSexp [Var "x"]; LSexp [Var "x"]];
    LSexp [Var "define"; Var "f";
      LSexp [Var "lambda"; LSexp [Var "x"];
        Var "x"]];
    LSexp [Var "f"; LInt 334];
  ] (ref {frame=Env.empty; enclosing=None});
  interpret [
    LSexp [Var "+"; LInt 42; LInt 2434];
    LSexp [Var "define"; Var "add100";
      LSexp [Var "lambda"; LSexp [Var "v"];
        LSexp [Var "+"; Var "v"; LInt 100]]];
    LSexp [Var "add100"; LInt 2434];
    LSexp [Var "+"; LInt 2; LInt 3];
  ] (ref {frame=Env.singleton "+" @@ VPrimitive {
    name="+"; arity=2; func=
    function
    | [VInt u;VInt v] ->
      VInt (u+v) | _ -> raise TypeError
  }; enclosing=None});

