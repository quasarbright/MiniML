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

/*
expr is supposed to be assigned to the lowest-precedence thing
left associativity:
  plusOp : '+' | '-'

  ePlus :
    | eTimes
    | ePlus plusOp eTimes

right associativity:
  exponentOp : '**'
  eExponent :
    | eLess
    | eLess exponentOp eExponent

nonassociative:
  eIf :
    | eOr
    | 'if' eIf 'then' eIf 'else' eIf
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
  | IF eIf THEN eIf ELSE eIf { EIf($2, $4, $6, full_span()) }

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
  | TIMES { Times }
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
  | unop eUnop { EPrim1($1, $2, full_span()) }

eApp :
  | eParen { $1 }
  | eApp eParen { EApp($1, $2, full_span()) }

eParen :
  | atom { $1 }
  | LPAREN expr RPAREN { $2 }

const :
  | INT { EInt($1, full_span()) }
  | TRUE { EBool(true, full_span()) }
  | FALSE { EBool(false, full_span()) }

atom :
  | IDENT { EId($1, full_span()) }
  | const { $1 }

expr : eLet { $1 }

program:
  expr { ($1, full_span()) }

%%