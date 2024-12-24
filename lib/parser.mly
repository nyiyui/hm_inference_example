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

%nonassoc NOT
%nonassoc IN
%nonassoc ELSE
%left EQUALS
%left PLUS
%left TIMES

%start <string Ast.expr> program

%%

program:
  | e = expr EOF { e }
  ;

expr:
  | i = INT { Int i }
  | x = ID { Var x }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | NOT; e = expr { OpUnary (Not, e) }
  | e1 = expr; PLUS; e2 = expr { OpBinary (Add, e1, e2) }
  | e1 = expr; TIMES; e2 = expr { OpBinary (Mul, e1, e2) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
  | LET; x = ID; EQUALS; e1 = expr; IN; e2 = expr { Let (x, e1, e2) }
  | LPAREN; e = expr; RPAREN { e }
  ;
