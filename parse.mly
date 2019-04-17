%token <int> NUMBER
%token <float> FLOAT
%token <string> STRING
%token <string> VARIABLE
%token LEFT_PAREN RIGHT_PAREN EOF

%start <Eval.exp option> prog
%%

prog:
  | EOF        {None}
  | e=exp      {Some e}
;
exp:
  | v=self_eval   {v}
  | s=VARIABLE    {Eval.Var s}
  | s=sexp        {Eval.LSexp s}
;
self_eval:
  | n=NUMBER      {Eval.LInt n}
  | s=STRING      {Eval.LString s}
  | f=FLOAT       {Eval.LFloat f}
;
sexp:
  | LEFT_PAREN ls=sexp_list RIGHT_PAREN {ls}
  | LEFT_PAREN RIGHT_PAREN  {[]}
;
sexp_list:
  | e=exp               {[e]}
  | e=exp ls=sexp_list  {e::ls}
;
