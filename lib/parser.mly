%{
  open Ast
%}

%token <int> INT
%token <string> ID
%token TRUE
%token FALSE
%token PLUS
%token TIMES
%token EQUALS
%token NOT
%token LET
%token IN
%token IF
%token THEN
%token ELSE
%token LPAREN
%token RPAREN
%token RARROW
%token EOF

%right RARROW
%nonassoc NOT
%nonassoc IN
%nonassoc ELSE
%left PLUS
%left TIMES

%start <string Ast.expr> program

%%

program:
  | e = expr EOF { e }
  ;

expr:
  | e1 = expr; e2 = expr_no_app %prec RARROW { Application (e1, e2) }
  | e = expr_no_app { e }
  ;

expr_no_app:
  | i = INT { Int i }
  | x = ID { Var x }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | NOT; e = expr { OpUnary (Not, e) }
  | e1 = expr; PLUS; e2 = expr { OpBinary (Add, e1, e2) }
  | e1 = expr; TIMES; e2 = expr { OpBinary (Mul, e1, e2) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
  | LET; x = ID; EQUALS; e1 = expr; IN; e2 = expr { Application (Closure (x, e2), e1) }
  | LPAREN; e = expr; RPAREN { e }
  | x = ID; RARROW; e = expr { Closure (x, e) }
  ;
