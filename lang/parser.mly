%{
open Exprs

let full_span () = (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())
let tok_span (start, endtok) = (Parsing.rhs_start_pos start, Parsing.rhs_end_pos endtok)
%}

%token <int64> INT
%token <string> IDENT
%token TRUE FALSE LPAREN RPAREN AND OR NOT EQ NEQ LT GT LTE GTE PLUS MINUS UNEGATE TIMES DIVIDE MODULO EOF LET IN IF THEN ELSE

%left ASSIGN
%nonassoc LET REC IN
%left CONDITIONAL
%nonassoc IF THEN ELSE
%left OR
%left AND
%left EQ NEQ
%left GTE LTE GT LT
%left MINUS PLUS
%left MODULO DIVIDE TIMES
%nonassoc NOT
%nonassoc UNEGATE
%left APPLY
%nonassoc GROUP
%nonassoc LPAREN RPAREN
%nonassoc ATOM
%nonassoc TRUE FALSE INT ID
%nonassoc EOF

%type <(Lexing.position * Lexing.position) Exprs.program> program

%start program



%%

const :
  | INT { EInt($1, full_span()) }
  | TRUE { EBool(true, full_span()) }
  | FALSE { EBool(false, full_span()) }

atom :
  | IDENT { EId($1, full_span()) }
  | const { $1 }

/*
ident :
  | IDENT { ($1, full_span()) }

idents :
  | ident { [$1] }
  | ident idents { $1::$2 }

bind :
  | idents EQ expr { ($1, $3, full_span()) }

let_bind :
  | LET bind IN expr { ELet($2, $4, full_span()) }

conditional :
  | IF expr THEN expr ELSE expr { EIf($2, $4, $6, full_span()) }

expr:
  | atom %prec ATOM { $1 }
  | let_bind %prec ASSIGN { $1 }
  | conditional %prec CONDITIONAL { $1 }
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
  | UNEGATE expr { EPrim1(UNegate, $2, full_span()) }
  | app %prec APPLY { $1 }
  | LPAREN expr RPAREN { $2 }


app :
  | expr expr %prec APPLY { EApp($1, $2, full_span()) }

*/

ident :
  IDENT { ($1, full_span()) }

idents :
  | ident { [$1] }
  | ident idents { $1::$2 }

bind :
  | idents EQ eLet { ($1, $3, full_span()) }

eLet :
  | eIf { $1 }
  | LET bind IN eLet { ELet($2, $4, full_span()) }

eIf :
  | eOr { $1 }
  | IF eOr THEN eOr ELSE eOr { EIf($2, $4, $6, full_span()) }

eOr :
  | eAnd { $1 }
  | eOr OR eAnd { EPrim2(Or, $1, $3, full_span()) }

eAnd :
  | eEq { $1 }
  | eAnd AND eEq { EPrim2(And, $1, $3, full_span()) }

eqOp :
  | EQ { Eq }
  | NEQ { Neq }

eEq :
  | eLess { $1 }
  | eEq eqOp eLess { EPrim2($2, $1, $3, full_span()) }

lessOp :
  | GT { Greater }
  | LT { Less }
  | LTE { LessEq }
  | GTE { GreaterEq }

eLess :
  | ePlus { $1 }
  | eLess lessOp ePlus { EPrim2($2, $1, $3, full_span()) }

plusOp :
  | PLUS { Plus }
  | MINUS { Minus }

ePlus :
  | eMul { $1 }
  | ePlus plusOp eMul { EPrim2($2, $1, $3, full_span()) }

mulOp :
  | TIMES  { Times }
  | DIVIDE { Divide }
  | MODULO { Modulo }

eMul :
  | eUnop { $1 }
  | eMul mulOp eUnop { EPrim2($2, $1, $3, full_span()) }

unop :
  | UNEGATE { UNegate }
  | NOT { Not }

eUnop :
  | eApp { $1 }
  | unop eApp { EPrim1($1, $2, full_span()) }

eApp :
  | eParen { $1 }
  | eApp eParen { EApp($1, $2, full_span()) }

eParen :
  | atom { $1 }
  | LPAREN eLet RPAREN { $2 }

expr : eLet { $1 }

program:
  expr { ($1, full_span()) }

%%