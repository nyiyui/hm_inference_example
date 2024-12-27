%{
  open Ast
%}

%token <int> INT
%token <string> ID_VALUE
%token <string> ID_TYPE
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
%nonassoc TRUE FALSE INT ID_VALUE ID_TYPE LPAREN

%start <string Ast.expr> program

%%

program:
  | e = expr EOF { e }
  ;

expr:
  | e = expr_op { e }
  | e1 = expr; e2 = expr_simple { Application (e1, e2) }
  | LET; x = ID_VALUE; EQUALS; e1 = expr; IN; e2 = expr { Let (x, e1, e2) }
  | x = ID_VALUE; RARROW; e = expr { Closure (x, e) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
  ;

expr_op:
  | NOT; e = expr_op { OpUnary (Not, e) }
  | e1 = expr_op; PLUS; e2 = expr_op { OpBinary (Add, e1, e2) }
  | e1 = expr_op; TIMES; e2 = expr_op { OpBinary (Mul, e1, e2) }
  | e = expr_simple { e }
  ;

expr_simple:
  | i = INT { Int i }
  | x = ID_VALUE { Var x }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | LPAREN; e = expr; RPAREN { e }
  ;
