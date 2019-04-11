let ( *> ) f g x = g (f x)
let ( <? ) f x y = f y x

let rec parse lexbuf =
  let result = Parse.prog Lexer.read lexbuf
  in match result with
  | Some v -> v :: (parse lexbuf)
  | None -> []

let env_global =
  let e = ref @@ Eval.setup_env ()
  in Eval.add_more_prims_env e; e

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
  interpret {|
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
        (define res (if (= v 1)
          1
          (* (fact (- v 1)) v)))
        (print res)
        res))
    (fact 5)
    (define collatz (lambda (v)
      (define loop (lambda (v step)
        (print v)
        (if (= v 1)
          (print (^ (^ "converged in " (string_of_int step)) " steps"))
          (if (= (mod v 2) 0)
            (loop (/ v 2) (+ step 1))
            (loop (+ ( * v 3) 1) (+ step 1))))))
      (loop v 0)))
    (collatz 3)
    (show_env)
    (define ls  (quote (1 2 3)))
    (print ls)
    (define ls2 (append ls ls))
    (print ls2)
    (print (car (cdr (cdr ls))))
    (define null? (lambda (v) (= v (quote ()))))
    (define member (lambda (ls v)
      (define loop (lambda (ls)
        (if (null? ls)
          false
          (if (= v (car ls))
            true
            (loop (cdr ls))))))
      (loop ls)))
    (member (quote (1 2 4 8)) (pow 2 3))
    (member (quote (1 2 4 8)) 5)
    (define squares_ngt (lambda (limit)
      (define loop (lambda (v ls)
        (define p (* v v))
        (if (> p limit)
          ls
          (loop (+ v 1) (append ls (list p))))))
      (loop 0 (quote ()))))
    (member (squares_ngt 100) (pow 2 3))
    (member (squares_ngt 100) (pow 2 6))
    (member (squares_ngt 100) (pow 2 8))
  |};
  interpreter ()
