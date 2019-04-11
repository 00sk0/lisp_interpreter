# Lisp-like language Interpreter

```bash
# run
$ make
```

* static scope
  ```lisp
  (define x 1)
  (define a (lambda () (print x)))
  (define b (lambda ()
    (define x 2)
    (a)))
  (b) ; prints 1
  ```
* other examples are in interpret.ml
* most of the function and special form names derive from OCaml, and may look strange.
