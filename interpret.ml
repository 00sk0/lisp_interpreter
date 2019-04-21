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

let interpreter ic =
  let rec loop input =
    try
      let input = input ^ "\n" ^ (input_line ic) in
      begin try
        interpret input;
      with Parse.Error -> loop input end;
      loop ""
    with
    | End_of_file | Exit -> ()
    | e ->
      prerr_endline @@ "\027[31mError : " ^  Printexc.to_string e ^ "\027[0m";
      prerr_endline @@ Printexc.get_backtrace ();
      loop ""
  in loop ""

let () =
  env_global := {!env_global with frame=
    Eval.Env.add "load_file" (Eval.VPrimitive (function
      | [VString file] ->
        let ic = open_in file in
        let str = really_input_string ic (in_channel_length ic) in
        interpret str;
        close_in ic; VUnit
      | _ -> raise Eval.TypeError)) !env_global.frame}

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
      ; list square numbers not greater than limit
      (define loop (lambda (v ls)
        (define p (* v v))
        (if (> p limit)
          ls
          (loop (+ v 1) (append ls (list p))))))
      (loop 0 (quote ()))))
    (member (squares_ngt 100) (pow 2 3)) ; 2^3 = 8
    (member (squares_ngt 100) (pow 2 6)) ; 2^6 = 8^2 <= 100
    (member (squares_ngt 100) (pow 2 8)) ; 2^8 = 16^2 > 100
    (define monte_pi (lambda (max)
      ; estimate pi using monte-carlo method
      (define rand (lambda ()
        (define x (random_float 1.))
        (define y (random_float 1.))
        (if (> (+. (*. x x) (*. y y)) 1.) 0. 1.)))
      (define loop (lambda (count sum)
        (if (= count max)
          (/. (*. 4. sum) (float_of_int max))
          (loop (+ count 1) (+. sum (rand))))))
      (define pi2 (loop 0 0.))
      (print "pi:" pi2)
      (define fabs (lambda (v)
        (if (> v 0.) v (*. v -1.))))
      (print "relative error:" (fabs (/. (-. pi2 pi) pi)))))
    (define time (lambda (v)
      (define tic (time_now))
      (v)
      (define toc (time_now))
      (-. toc tic)))
    (time_internal (monte_pi 10000))
    (time (lambda () (monte_pi 10000)))
    (load_file "example/list.lisplike")
    (filter (lambda (v) (= (mod v 2) 0)) (squares_ngt 100))
  |};
  interpreter stdin;
  print_endline "exit."
