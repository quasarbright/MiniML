type token =
  | INT of (int64)
  | IDENT of (string)
  | TRUE
  | FALSE
  | LPAREN
  | RPAREN
  | AND
  | OR
  | NOT
  | EQ
  | NEQ
  | LT
  | GT
  | LTE
  | GTE
  | PLUS
  | MINUS
  | UNEGATE
  | TIMES
  | DIVIDE
  | MODULO
  | EOF
  | LET
  | IN
  | IF
  | THEN
  | ELSE

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Exprs

let full_span () = (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())
let tok_span (start, endtok) = (Parsing.rhs_start_pos start, Parsing.rhs_end_pos endtok)
# 38 "pp.ml"
let yytransl_const = [|
  259 (* TRUE *);
  260 (* FALSE *);
  261 (* LPAREN *);
  262 (* RPAREN *);
  263 (* AND *);
  264 (* OR *);
  265 (* NOT *);
  266 (* EQ *);
  267 (* NEQ *);
  268 (* LT *);
  269 (* GT *);
  270 (* LTE *);
  271 (* GTE *);
  272 (* PLUS *);
  273 (* MINUS *);
  274 (* UNEGATE *);
  275 (* TIMES *);
  276 (* DIVIDE *);
  277 (* MODULO *);
    0 (* EOF *);
  278 (* LET *);
  279 (* IN *);
  280 (* IF *);
  281 (* THEN *);
  282 (* ELSE *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\002\000\003\000\003\000\004\000\005\000\005\000\
\006\000\008\000\008\000\007\000\007\000\009\000\009\000\010\000\
\010\000\012\000\012\000\011\000\011\000\014\000\014\000\014\000\
\014\000\013\000\013\000\016\000\016\000\015\000\015\000\018\000\
\018\000\018\000\017\000\017\000\020\000\020\000\019\000\019\000\
\021\000\021\000\022\000\022\000\023\000\001\000\000\000"

let yylen = "\002\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\002\000\
\003\000\001\000\004\000\001\000\006\000\001\000\003\000\001\000\
\003\000\001\000\001\000\001\000\003\000\001\000\001\000\001\000\
\001\000\001\000\003\000\001\000\001\000\001\000\003\000\001\000\
\001\000\001\000\001\000\003\000\001\000\001\000\001\000\003\000\
\001\000\002\000\001\000\003\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\001\000\004\000\002\000\003\000\000\000\047\000\
\005\000\043\000\045\000\046\000\000\000\000\000\010\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\041\000\006\000\000\000\000\000\000\000\000\000\044\000\000\000\
\000\000\018\000\019\000\000\000\023\000\022\000\024\000\025\000\
\000\000\028\000\029\000\000\000\032\000\033\000\034\000\000\000\
\038\000\037\000\000\000\042\000\008\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\009\000\
\011\000\000\000\000\000\000\000"

let yydgoto = "\002\000\
\008\000\009\000\010\000\027\000\028\000\029\000\015\000\016\000\
\017\000\018\000\019\000\036\000\020\000\041\000\021\000\044\000\
\022\000\048\000\023\000\051\000\024\000\025\000\012\000"

let yysindex = "\018\000\
\013\255\000\000\000\000\000\000\000\000\000\000\002\255\000\000\
\000\000\000\000\000\000\000\000\023\255\013\255\000\000\015\255\
\038\255\046\255\045\255\037\255\143\255\163\255\004\255\013\255\
\000\000\000\000\023\255\048\255\056\255\019\255\000\000\013\255\
\013\255\000\000\000\000\013\255\000\000\000\000\000\000\000\000\
\013\255\000\000\000\000\013\255\000\000\000\000\000\000\013\255\
\000\000\000\000\013\255\000\000\000\000\007\255\007\255\013\255\
\046\255\045\255\037\255\143\255\163\255\004\255\013\255\000\000\
\000\000\250\254\013\255\038\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\094\255\199\255\034\255\193\255\162\255\022\255\099\255\057\255\
\000\000\000\000\051\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\215\255\155\255\204\255\183\255\141\255\120\255\078\255\000\000\
\000\000\000\000\000\000\115\255"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\054\000\000\000\177\000\000\000\
\243\255\070\000\075\000\000\000\087\000\000\000\088\000\000\000\
\098\000\000\000\096\000\000\000\099\000\255\255\000\000"

let yytablesize = 241
let yytable = "\011\000\
\030\000\032\000\003\000\004\000\005\000\006\000\007\000\003\000\
\004\000\005\000\006\000\007\000\049\000\003\000\004\000\005\000\
\006\000\007\000\001\000\067\000\031\000\050\000\052\000\013\000\
\026\000\014\000\032\000\030\000\030\000\030\000\014\000\030\000\
\030\000\030\000\030\000\030\000\030\000\030\000\030\000\016\000\
\016\000\016\000\066\000\056\000\030\000\032\000\030\000\030\000\
\037\000\038\000\039\000\040\000\033\000\068\000\034\000\035\000\
\016\000\054\000\016\000\016\000\007\000\052\000\039\000\039\000\
\039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
\039\000\039\000\039\000\039\000\039\000\039\000\055\000\039\000\
\053\000\039\000\039\000\040\000\040\000\040\000\040\000\040\000\
\040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
\040\000\040\000\040\000\012\000\040\000\057\000\040\000\040\000\
\035\000\035\000\035\000\058\000\035\000\035\000\035\000\035\000\
\035\000\035\000\035\000\035\000\012\000\035\000\035\000\035\000\
\013\000\035\000\059\000\035\000\035\000\036\000\036\000\036\000\
\060\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
\036\000\013\000\036\000\036\000\036\000\061\000\036\000\062\000\
\036\000\036\000\031\000\031\000\031\000\063\000\031\000\031\000\
\031\000\031\000\031\000\031\000\031\000\031\000\042\000\043\000\
\017\000\017\000\017\000\031\000\000\000\031\000\031\000\026\000\
\026\000\026\000\000\000\026\000\026\000\026\000\026\000\026\000\
\026\000\017\000\000\000\017\000\017\000\045\000\046\000\047\000\
\026\000\000\000\026\000\026\000\027\000\027\000\027\000\000\000\
\027\000\027\000\027\000\027\000\027\000\027\000\020\000\020\000\
\020\000\000\000\020\000\020\000\014\000\027\000\014\000\027\000\
\027\000\021\000\021\000\021\000\000\000\021\000\021\000\020\000\
\000\000\020\000\020\000\000\000\015\000\014\000\015\000\014\000\
\014\000\000\000\021\000\000\000\021\000\021\000\064\000\065\000\
\000\000\000\000\000\000\000\000\000\000\015\000\000\000\015\000\
\015\000"

let yycheck = "\001\000\
\014\000\008\001\001\001\002\001\003\001\004\001\005\001\001\001\
\002\001\003\001\004\001\005\001\009\001\001\001\002\001\003\001\
\004\001\005\001\001\000\026\001\006\001\018\001\024\000\022\001\
\002\001\024\001\008\001\006\001\007\001\008\001\024\001\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\006\001\
\007\001\008\001\056\000\025\001\023\001\008\001\025\001\026\001\
\012\001\013\001\014\001\015\001\007\001\067\000\010\001\011\001\
\023\001\010\001\025\001\026\001\010\001\063\000\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\023\001\023\001\
\027\000\025\001\026\001\006\001\007\001\008\001\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\006\001\023\001\032\000\025\001\026\001\
\006\001\007\001\008\001\033\000\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\023\001\019\001\020\001\021\001\
\006\001\023\001\036\000\025\001\026\001\006\001\007\001\008\001\
\041\000\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\017\001\023\001\019\001\020\001\021\001\044\000\023\001\048\000\
\025\001\026\001\006\001\007\001\008\001\051\000\010\001\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\016\001\017\001\
\006\001\007\001\008\001\023\001\255\255\025\001\026\001\006\001\
\007\001\008\001\255\255\010\001\011\001\012\001\013\001\014\001\
\015\001\023\001\255\255\025\001\026\001\019\001\020\001\021\001\
\023\001\255\255\025\001\026\001\006\001\007\001\008\001\255\255\
\010\001\011\001\012\001\013\001\014\001\015\001\006\001\007\001\
\008\001\255\255\010\001\011\001\006\001\023\001\008\001\025\001\
\026\001\006\001\007\001\008\001\255\255\010\001\011\001\023\001\
\255\255\025\001\026\001\255\255\006\001\023\001\008\001\025\001\
\026\001\255\255\023\001\255\255\025\001\026\001\054\000\055\000\
\255\255\255\255\255\255\255\255\255\255\023\001\255\255\025\001\
\026\001"

let yynames_const = "\
  TRUE\000\
  FALSE\000\
  LPAREN\000\
  RPAREN\000\
  AND\000\
  OR\000\
  NOT\000\
  EQ\000\
  NEQ\000\
  LT\000\
  GT\000\
  LTE\000\
  GTE\000\
  PLUS\000\
  MINUS\000\
  UNEGATE\000\
  TIMES\000\
  DIVIDE\000\
  MODULO\000\
  EOF\000\
  LET\000\
  IN\000\
  IF\000\
  THEN\000\
  ELSE\000\
  "

let yynames_block = "\
  INT\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int64) in
    Obj.repr(
# 40 "parser.mly"
        ( EInt(_1, full_span()) )
# 238 "pp.ml"
               : 'const))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "parser.mly"
         ( EBool(true, full_span()) )
# 244 "pp.ml"
               : 'const))
; (fun __caml_parser_env ->
    Obj.repr(
# 42 "parser.mly"
          ( EBool(false, full_span()) )
# 250 "pp.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 45 "parser.mly"
          ( EId(_1, full_span()) )
# 257 "pp.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'const) in
    Obj.repr(
# 46 "parser.mly"
          ( _1 )
# 264 "pp.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 94 "parser.mly"
        ( (_1, full_span()) )
# 271 "pp.ml"
               : 'ident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 97 "parser.mly"
          ( [_1] )
# 278 "pp.ml"
               : 'idents))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'ident) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'idents) in
    Obj.repr(
# 98 "parser.mly"
                 ( _1::_2 )
# 286 "pp.ml"
               : 'idents))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'idents) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'eIf) in
    Obj.repr(
# 101 "parser.mly"
                  ( (_1, _3, full_span()) )
# 294 "pp.ml"
               : 'bind))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'eIf) in
    Obj.repr(
# 104 "parser.mly"
        ( _1 )
# 301 "pp.ml"
               : 'eLet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'bind) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'eIf) in
    Obj.repr(
# 105 "parser.mly"
                    ( ELet(_2, _4, full_span()) )
# 309 "pp.ml"
               : 'eLet))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'eOr) in
    Obj.repr(
# 108 "parser.mly"
        ( _1 )
# 316 "pp.ml"
               : 'eIf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'eOr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'eOr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'eOr) in
    Obj.repr(
# 109 "parser.mly"
                             ( EIf(_2, _4, %6, full_span()) )
# 325 "pp.ml"
               : 'eIf))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'eAnd) in
    Obj.repr(
# 112 "parser.mly"
         ( _1 )
# 332 "pp.ml"
               : 'eOr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'eOr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'eAnd) in
    Obj.repr(
# 113 "parser.mly"
                ( EPrim2(Or, _1, _3, full_span()) )
# 340 "pp.ml"
               : 'eOr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'eEq) in
    Obj.repr(
# 116 "parser.mly"
        ( _1 )
# 347 "pp.ml"
               : 'eAnd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'eAnd) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'eEq) in
    Obj.repr(
# 117 "parser.mly"
                 ( EPrim2(And, _1, _3, full_span()) )
# 355 "pp.ml"
               : 'eAnd))
; (fun __caml_parser_env ->
    Obj.repr(
# 120 "parser.mly"
       ( Eq )
# 361 "pp.ml"
               : 'eqOp))
; (fun __caml_parser_env ->
    Obj.repr(
# 121 "parser.mly"
        ( Neq )
# 367 "pp.ml"
               : 'eqOp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'eLess) in
    Obj.repr(
# 124 "parser.mly"
          ( _1 )
# 374 "pp.ml"
               : 'eEq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'eEq) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'eqOp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'eLess) in
    Obj.repr(
# 125 "parser.mly"
                   ( EPrim2(_2, _1, _3, full_span) )
# 383 "pp.ml"
               : 'eEq))
; (fun __caml_parser_env ->
    Obj.repr(
# 128 "parser.mly"
       ( Greater )
# 389 "pp.ml"
               : 'lessOp))
; (fun __caml_parser_env ->
    Obj.repr(
# 129 "parser.mly"
       ( Less )
# 395 "pp.ml"
               : 'lessOp))
; (fun __caml_parser_env ->
    Obj.repr(
# 130 "parser.mly"
        ( LessEq )
# 401 "pp.ml"
               : 'lessOp))
; (fun __caml_parser_env ->
    Obj.repr(
# 131 "parser.mly"
        ( GreaterEq )
# 407 "pp.ml"
               : 'lessOp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ePlus) in
    Obj.repr(
# 134 "parser.mly"
          ( _1 )
# 414 "pp.ml"
               : 'eLess))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'eLess) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'lessOp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ePlus) in
    Obj.repr(
# 135 "parser.mly"
                       ( EPrim2(_2, _1, _3, full_span()) )
# 423 "pp.ml"
               : 'eLess))
; (fun __caml_parser_env ->
    Obj.repr(
# 138 "parser.mly"
         ( Plus )
# 429 "pp.ml"
               : 'plusOp))
; (fun __caml_parser_env ->
    Obj.repr(
# 139 "parser.mly"
          ( Minus )
# 435 "pp.ml"
               : 'plusOp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'eMul) in
    Obj.repr(
# 142 "parser.mly"
         ( _1 )
# 442 "pp.ml"
               : 'ePlus))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ePlus) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'plusOp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'eMul) in
    Obj.repr(
# 143 "parser.mly"
                      ( EPrim2(_2, _1, _3, full_span()) )
# 451 "pp.ml"
               : 'ePlus))
; (fun __caml_parser_env ->
    Obj.repr(
# 146 "parser.mly"
           ( Times )
# 457 "pp.ml"
               : 'mulOp))
; (fun __caml_parser_env ->
    Obj.repr(
# 147 "parser.mly"
           ( Divide )
# 463 "pp.ml"
               : 'mulOp))
; (fun __caml_parser_env ->
    Obj.repr(
# 148 "parser.mly"
           ( Modulo )
# 469 "pp.ml"
               : 'mulOp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'eUnop) in
    Obj.repr(
# 151 "parser.mly"
          ( _1 )
# 476 "pp.ml"
               : 'eMul))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'eMul) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'mulOp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'eUnop) in
    Obj.repr(
# 152 "parser.mly"
                     ( EPrim2(_2, _1, _3, full_span()) )
# 485 "pp.ml"
               : 'eMul))
; (fun __caml_parser_env ->
    Obj.repr(
# 155 "parser.mly"
            ( UNegate )
# 491 "pp.ml"
               : 'unop))
; (fun __caml_parser_env ->
    Obj.repr(
# 156 "parser.mly"
        ( Not )
# 497 "pp.ml"
               : 'unop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'eApp) in
    Obj.repr(
# 159 "parser.mly"
         ( _1 )
# 504 "pp.ml"
               : 'eUnop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'eUnop) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'unop) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'eApp) in
    Obj.repr(
# 160 "parser.mly"
                    ( EPrim1(_1, _2, full_span()) )
# 513 "pp.ml"
               : 'eUnop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'eParen) in
    Obj.repr(
# 163 "parser.mly"
           ( _1 )
# 520 "pp.ml"
               : 'eApp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'eApp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'eParen) in
    Obj.repr(
# 164 "parser.mly"
                ( EApp(_1, _2, full_span) )
# 528 "pp.ml"
               : 'eApp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 167 "parser.mly"
         ( _1 )
# 535 "pp.ml"
               : 'eParen))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'eLet) in
    Obj.repr(
# 168 "parser.mly"
                       ( _2 )
# 542 "pp.ml"
               : 'eParen))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'eParen) in
    Obj.repr(
# 170 "parser.mly"
              ( _1 )
# 549 "pp.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 173 "parser.mly"
       ( (_1, full_span()) )
# 556 "pp.ml"
               : (Lexing.position * Lexing.position) Exprs.program))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : (Lexing.position * Lexing.position) Exprs.program)
;;
