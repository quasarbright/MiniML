%{
open Exprs

let full_span() = (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())
let tok_span(start, endtok) = (Parsing.rhs_start_pos start, Parsing.rhs_end_pos endtok)
%}

%token <int64> INT
%token TRUE FALSE LPAREN RPAREN AND OR NOT EQ NEQ LT GT LTE GTE PLUS MINUS UNEGATE TIMES DIVIDE MODULO EOF

%left OR
%left AND
%left EQ NEQ
%left GTE LTE GT LT
%left MINUS PLUS
%left MODULO DIVIDE TIMES
%nonassoc NOT
%nonassoc UNEGATE
%nonassoc GROUP
%nonassoc LPAREN RPAREN
%nonassoc ATOM
%nonassoc TRUE FALSE INT
%nonassoc EOF

%type <(Lexing.position * Lexing.position) Exprs.program> program

%start program



%%

expr:
  | INT { EInt($1, full_span()) }
  | TRUE { EBool(true, full_span()) }
  | FALSE { EBool(false, full_span()) }
  | expr OR expr { EPrim2(Or, $1, $3, full_span()) }
  | expr AND expr { EPrim2(And, $1, $3, full_span()) }
  | expr EQ expr { EPrim2(Eq, $1, $3, full_span()) }
  | expr NEQ expr { EPrim2(Neq, $1, $3, full_span()) }
  | expr GTE expr { EPrim2(GreaterEq, $1, $3, full_span()) }
  | expr LTE expr { EPrim2(LessEq, $1, $3, full_span()) }
  | expr GT expr { EPrim2(Greater, $1, $3, full_span()) }
  | expr LT expr { EPrim2(Less, $1, $3, full_span()) }
  | expr MINUS expr { EPrim2(Minus, $1, $3, full_span()) }
  | expr PLUS expr { EPrim2(Plus, $1, $3, full_span()) }
  | expr MODULO expr { EPrim2(Modulo, $1, $3, full_span()) }
  | expr DIVIDE expr { EPrim2(Divide, $1, $3, full_span()) }
  | expr TIMES expr { EPrim2(Times, $1, $3, full_span()) }
  | NOT expr { EPrim1(Not, $2, full_span()) }
  | UNEGATE expr { EPrim1(Not, $2, full_span()) }
  | LPAREN expr RPAREN %prec GROUP { $2 }

program:
  expr EOF { ($1, full_span()) }

%%