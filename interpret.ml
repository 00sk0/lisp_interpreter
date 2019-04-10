let ( *> ) f g x = g (f x)
let ( <? ) f x y = f y x

let rec parse lexbuf =
  let result = Parse.prog Lexer.read lexbuf
  in match result with
  | Some v -> v :: (parse lexbuf)
  | None -> []

let env_global = ref @@ Eval.setup_env ()
let rec add_more_prims () =
  let open Eval in
  let {frame;_} as env = !env_global in
  let prim_fun = [
    "show_env", `n_U (fun () ->
      print_endline
      @@ string_of_env ~ln:true !env_global);
    "reset_env", `n_U (fun () ->
      env_global := Eval.setup_env ();
      add_more_prims ());
    "help", `n_U (fun () ->
      Printf.printf "lisp_interpreter\n");
  ] in
  env_global := {env with frame=add_vprims_to_frame frame prim_fun}
let () = add_more_prims ()

let interpret str = (
  Lexing.from_string
  *> parse
  *> Eval.interpret <? env_global) str;
  Printf.printf "\n\n%!"

let interpreter () =
  try
    let input = ref "" in
    while true do
      input := !input ^ " " ^ (read_line ());
      try
        interpret !input;
        input := ""
      with Parse.Error -> ()
    done
  with End_of_file -> print_endline "exit."

let () =
  interpret "
    (define x 128)
    (define add100 (lambda (v) (+ v 100)))
    (add100 the_answer_to_everything)
    (define foo (lambda (v)
      (define bar
        (define mul (lambda (u v) (* u v)))
        (lambda (u v) (mul u v)))
      (bar v 2)
    ))
    (foo 24)
    (define baz (lambda (f g)
      (lambda (v)
        (print v)
        (define v (f v))
        (print v)
        (define v (g v))
        (print v)
        v)))
    ((baz not not) true)
    (debug true)
    ((baz
      (lambda (v) (+ v 1))
      (lambda (v) (* v 2))) 1)
    (debug false)
    (define fact
      (lambda (v)
        (if (= v 0)
          1
          (* (fact (- v 1)) v))))
  ";
  interpreter ()
