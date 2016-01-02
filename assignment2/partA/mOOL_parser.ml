type token =
  | AND
  | OR
  | EQ
  | NEQ
  | PLUS
  | MINUS
  | MULTIPLY
  | DIVIDE
  | GRE
  | GEQ
  | LE
  | LEQ
  | NEG
  | ASSIGN
  | INT_KWORD
  | BOOL_KWORD
  | STRING_KWORD
  | VOID_KWORD
  | TRUE_KWORD
  | FALSE_KWORD
  | CLASS_KWORD
  | WHILE_KWORD
  | IF_KWORD
  | ELSE_KWORD
  | RETURN_KWORD
  | THIS_KWORD
  | SUPER_KWORD
  | NULL_KWORD
  | NEW_KWORD
  | MAIN_KWORD
  | READ_KWORD
  | PRINT_KWORD
  | PRIVATE_KWORD
  | EXTENDS_KWORD
  | INTEGER_LITERAL of (int)
  | STRING_LITERAL of (string)
  | VAR_IDENTIFIER of (string)
  | CLASS_IDENTIFIER of (string)
  | NEWLINE
  | OPAREN
  | CPAREN
  | OBRACE
  | CBRACE
  | SEMICOLON
  | DOT
  | COMMA
  | COMMENT_LINE
  | COMMENT_OPEN
  | COMMENT_CLOSE
  | EOF

open Parsing;;
let _ = parse_error;;
# 8 "mOOL_parser.mly"


  open Printf
  open MOOL_structs

  let get_pos x = 
	Parsing.rhs_start_pos x
	
   let cnt = ref 0 
   let fresh_label () = (cnt:=!cnt+1; !cnt)
   
   let report_error pos s =
   print_string ("\nFile \"" ^ pos.Lexing.pos_fname ^ "\", line " ^ 
		(string_of_int pos.Lexing.pos_lnum) ^", col "^
    	(string_of_int (pos.Lexing.pos_cnum - pos.Lexing.pos_bol))^ ": "
        ^ s ^ "\n"); flush stdout;
  failwith "Error detected"
  
# 75 "mOOL_parser.ml"
let yytransl_const = [|
  257 (* AND *);
  258 (* OR *);
  259 (* EQ *);
  260 (* NEQ *);
  261 (* PLUS *);
  262 (* MINUS *);
  263 (* MULTIPLY *);
  264 (* DIVIDE *);
  265 (* GRE *);
  266 (* GEQ *);
  267 (* LE *);
  268 (* LEQ *);
  269 (* NEG *);
  270 (* ASSIGN *);
  271 (* INT_KWORD *);
  272 (* BOOL_KWORD *);
  273 (* STRING_KWORD *);
  274 (* VOID_KWORD *);
  275 (* TRUE_KWORD *);
  276 (* FALSE_KWORD *);
  277 (* CLASS_KWORD *);
  278 (* WHILE_KWORD *);
  279 (* IF_KWORD *);
  280 (* ELSE_KWORD *);
  281 (* RETURN_KWORD *);
  282 (* THIS_KWORD *);
  283 (* SUPER_KWORD *);
  284 (* NULL_KWORD *);
  285 (* NEW_KWORD *);
  286 (* MAIN_KWORD *);
  287 (* READ_KWORD *);
  288 (* PRINT_KWORD *);
  289 (* PRIVATE_KWORD *);
  290 (* EXTENDS_KWORD *);
  295 (* NEWLINE *);
  296 (* OPAREN *);
  297 (* CPAREN *);
  298 (* OBRACE *);
  299 (* CBRACE *);
  300 (* SEMICOLON *);
  301 (* DOT *);
  302 (* COMMA *);
  303 (* COMMENT_LINE *);
  304 (* COMMENT_OPEN *);
  305 (* COMMENT_CLOSE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  291 (* INTEGER_LITERAL *);
  292 (* STRING_LITERAL *);
  293 (* VAR_IDENTIFIER *);
  294 (* CLASS_IDENTIFIER *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\005\000\005\000\006\000\003\000\003\000\009\000\
\009\000\010\000\010\000\004\000\014\000\015\000\015\000\008\000\
\008\000\017\000\017\000\016\000\016\000\016\000\016\000\016\000\
\011\000\011\000\018\000\018\000\012\000\012\000\019\000\019\000\
\007\000\007\000\020\000\020\000\020\000\020\000\021\000\021\000\
\021\000\021\000\021\000\021\000\021\000\021\000\021\000\025\000\
\025\000\013\000\013\000\026\000\022\000\022\000\022\000\022\000\
\022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
\022\000\022\000\022\000\023\000\023\000\023\000\023\000\023\000\
\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
\024\000\024\000\027\000\027\000\000\000"

let yylen = "\002\000\
\002\000\005\000\000\000\002\000\007\000\000\000\001\000\001\000\
\002\000\000\000\001\000\009\000\001\000\009\000\010\000\000\000\
\001\000\001\000\002\000\001\000\001\000\001\000\001\000\001\000\
\000\000\001\000\002\000\004\000\000\000\001\000\003\000\004\000\
\000\000\001\000\004\000\005\000\001\000\003\000\003\000\002\000\
\011\000\007\000\004\000\005\000\005\000\006\000\005\000\000\000\
\001\000\001\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\002\000\002\000\001\000\003\000\001\000\001\000\001\000\001\000\
\004\000\001\000\001\000\001\000\001\000\004\000\004\000\003\000\
\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\085\000\000\000\000\000\000\000\001\000\
\008\000\000\000\000\000\000\000\009\000\000\000\000\000\000\000\
\000\000\000\000\002\000\004\000\000\000\000\000\021\000\020\000\
\022\000\023\000\011\000\024\000\000\000\000\000\037\000\000\000\
\034\000\000\000\000\000\000\000\000\000\000\000\018\000\000\000\
\000\000\000\000\013\000\000\000\000\000\027\000\000\000\005\000\
\000\000\000\000\019\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\035\000\000\000\000\000\000\000\
\028\000\000\000\036\000\000\000\071\000\072\000\000\000\000\000\
\000\000\074\000\075\000\076\000\000\000\000\000\000\000\070\000\
\069\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\040\000\077\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\012\000\
\000\000\051\000\000\000\000\000\031\000\000\000\000\000\000\000\
\000\000\000\000\066\000\065\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\039\000\000\000\000\000\000\000\000\000\000\000\000\000\080\000\
\000\000\000\000\000\000\000\000\000\000\032\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\063\000\064\000\000\000\000\000\000\000\000\000\000\000\068\000\
\078\000\000\000\000\000\000\000\043\000\000\000\000\000\000\000\
\000\000\014\000\000\000\000\000\073\000\044\000\045\000\047\000\
\000\000\000\000\015\000\000\000\000\000\046\000\042\000\000\000\
\000\000\000\000\000\000\041\000"

let yydgoto = "\002\000\
\004\000\005\000\008\000\015\000\017\000\009\000\029\000\037\000\
\010\000\030\000\034\000\062\000\083\000\096\000\031\000\032\000\
\041\000\036\000\064\000\033\000\085\000\138\000\098\000\139\000\
\000\000\000\000\140\000"

let yysindex = "\001\000\
\246\254\000\000\253\254\000\000\039\255\020\255\045\255\000\000\
\000\000\039\255\076\255\061\255\000\000\067\255\055\255\065\255\
\059\255\066\255\000\000\000\000\120\255\147\255\000\000\000\000\
\000\000\000\000\000\000\000\000\120\255\147\255\000\000\070\255\
\000\000\068\255\070\255\069\255\090\255\147\255\000\000\070\255\
\120\255\070\255\000\000\245\254\092\255\000\000\147\255\000\000\
\070\255\072\255\000\000\042\255\147\255\120\255\147\255\070\255\
\115\255\147\255\120\255\116\255\000\000\026\000\070\255\147\255\
\000\000\139\255\000\000\114\255\000\000\000\000\121\255\142\255\
\143\000\000\000\000\000\000\000\145\255\163\255\165\255\000\000\
\000\000\175\000\166\255\196\255\026\000\236\254\168\255\070\255\
\178\255\147\255\201\000\201\000\201\000\201\000\000\000\000\000\
\003\000\252\254\181\255\070\255\201\000\185\255\071\000\000\000\
\201\000\000\000\201\000\070\255\000\000\184\255\147\255\026\000\
\083\000\095\000\000\000\000\000\201\000\201\000\201\000\201\000\
\201\000\201\000\201\000\201\000\201\000\201\000\201\000\201\000\
\000\000\201\000\070\255\188\255\190\255\136\000\239\000\000\000\
\015\000\023\001\193\255\189\255\223\255\000\000\026\000\201\255\
\197\255\200\255\033\001\033\001\124\255\124\255\063\255\063\255\
\000\000\000\000\124\255\124\255\124\255\124\255\206\255\000\000\
\000\000\204\255\208\255\252\254\000\000\209\255\201\000\201\000\
\207\255\000\000\026\000\026\000\000\000\000\000\000\000\000\000\
\023\001\027\000\000\000\212\255\215\255\000\000\000\000\217\255\
\214\255\026\000\253\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\041\001\000\000\000\000\000\000\
\000\000\042\001\000\000\002\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\007\000\019\000\000\000\000\000\
\000\000\000\000\000\000\000\000\013\000\000\000\000\000\000\000\
\000\000\000\000\000\000\023\000\000\000\000\000\000\000\000\000\
\022\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\019\000\024\000\091\000\000\000\
\000\000\019\000\000\000\000\000\000\000\000\000\000\000\220\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\255\254\025\000\000\000\000\000\000\000\
\000\000\091\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\140\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\028\000\000\000\000\000\000\000\091\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\028\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\001\255\000\000\029\000\006\255\000\000\000\000\000\000\
\000\000\000\000\004\255\011\255\017\255\064\255\186\255\213\255\
\000\000\000\000\158\255\192\255\199\255\205\255\000\000\000\000\
\000\000\000\000\000\000\167\255\000\000\024\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\032\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\098\001\000\000\000\000\
\000\000\236\255\021\000\182\255\172\255\224\255\249\255\241\255\
\000\000\000\000\000\000\030\000\000\000\255\255\198\255\169\000\
\000\000\000\000\000\000"

let yytablesize = 557
let yytable = "\044\000\
\106\000\001\000\046\000\086\000\054\000\054\000\035\000\050\000\
\038\000\052\000\003\000\053\000\053\000\040\000\042\000\112\000\
\057\000\055\000\055\000\107\000\038\000\039\000\049\000\065\000\
\108\000\040\000\086\000\144\000\053\000\084\000\087\000\056\000\
\054\000\051\000\006\000\130\000\143\000\035\000\077\000\063\000\
\131\000\083\000\035\000\077\000\054\000\068\000\083\000\054\000\
\088\000\054\000\068\000\053\000\084\000\086\000\053\000\110\000\
\053\000\055\000\169\000\007\000\055\000\011\000\055\000\073\000\
\056\000\056\000\102\000\133\000\073\000\123\000\124\000\097\000\
\084\000\060\000\063\000\141\000\164\000\084\000\066\000\084\000\
\103\000\058\000\012\000\061\000\086\000\059\000\180\000\181\000\
\067\000\113\000\114\000\115\000\116\000\014\000\016\000\063\000\
\018\000\019\000\160\000\134\000\021\000\187\000\020\000\137\000\
\056\000\022\000\043\000\056\000\045\000\056\000\084\000\053\000\
\086\000\086\000\047\000\147\000\148\000\149\000\150\000\151\000\
\152\000\153\000\154\000\155\000\156\000\157\000\158\000\086\000\
\121\000\122\000\123\000\124\000\048\000\055\000\023\000\024\000\
\025\000\026\000\084\000\084\000\067\000\067\000\067\000\067\000\
\067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
\027\000\084\000\058\000\090\000\068\000\028\000\057\000\057\000\
\091\000\023\000\024\000\025\000\026\000\177\000\178\000\079\000\
\079\000\079\000\079\000\079\000\079\000\079\000\079\000\079\000\
\079\000\079\000\079\000\089\000\067\000\092\000\099\000\067\000\
\028\000\067\000\061\000\061\000\061\000\061\000\061\000\061\000\
\058\000\058\000\061\000\061\000\061\000\061\000\057\000\059\000\
\059\000\057\000\100\000\057\000\101\000\060\000\060\000\079\000\
\104\000\105\000\079\000\109\000\079\000\062\000\062\000\062\000\
\062\000\062\000\062\000\111\000\132\000\062\000\062\000\062\000\
\062\000\135\000\061\000\142\000\161\000\061\000\162\000\061\000\
\058\000\166\000\167\000\058\000\168\000\058\000\171\000\059\000\
\185\000\172\000\059\000\170\000\059\000\060\000\173\000\174\000\
\060\000\179\000\060\000\175\000\176\000\062\000\183\000\186\000\
\062\000\184\000\062\000\117\000\118\000\119\000\120\000\121\000\
\122\000\123\000\124\000\125\000\126\000\127\000\128\000\117\000\
\118\000\119\000\120\000\121\000\122\000\123\000\124\000\125\000\
\126\000\127\000\128\000\117\000\118\000\119\000\120\000\121\000\
\122\000\123\000\124\000\125\000\126\000\127\000\128\000\188\000\
\006\000\007\000\159\000\003\000\069\000\070\000\129\000\071\000\
\072\000\033\000\073\000\074\000\075\000\076\000\077\000\016\000\
\078\000\079\000\165\000\025\000\080\000\081\000\043\000\026\000\
\017\000\082\000\038\000\050\000\081\000\082\000\182\000\117\000\
\118\000\119\000\120\000\121\000\122\000\123\000\124\000\125\000\
\126\000\127\000\128\000\117\000\118\000\119\000\120\000\121\000\
\122\000\123\000\124\000\125\000\126\000\127\000\128\000\117\000\
\118\000\119\000\120\000\121\000\122\000\123\000\124\000\125\000\
\126\000\127\000\128\000\013\000\000\000\029\000\029\000\136\000\
\029\000\029\000\000\000\029\000\029\000\029\000\029\000\029\000\
\000\000\029\000\029\000\145\000\000\000\029\000\029\000\029\000\
\000\000\000\000\029\000\000\000\000\000\000\000\000\000\146\000\
\117\000\118\000\119\000\120\000\121\000\122\000\123\000\124\000\
\125\000\126\000\127\000\128\000\093\000\000\000\000\000\000\000\
\000\000\000\000\000\000\094\000\000\000\000\000\000\000\000\000\
\000\000\069\000\070\000\000\000\000\000\000\000\000\000\000\000\
\074\000\075\000\076\000\077\000\000\000\000\000\000\000\000\000\
\163\000\080\000\081\000\043\000\093\000\000\000\082\000\000\000\
\000\000\000\000\095\000\094\000\000\000\023\000\024\000\025\000\
\026\000\069\000\070\000\000\000\000\000\000\000\000\000\000\000\
\074\000\075\000\076\000\077\000\000\000\000\000\093\000\000\000\
\000\000\080\000\081\000\043\000\028\000\094\000\082\000\000\000\
\000\000\000\000\000\000\069\000\070\000\000\000\000\000\000\000\
\000\000\000\000\074\000\075\000\076\000\077\000\000\000\000\000\
\000\000\000\000\000\000\080\000\081\000\043\000\030\000\030\000\
\082\000\030\000\030\000\000\000\030\000\030\000\030\000\030\000\
\030\000\000\000\030\000\030\000\000\000\000\000\030\000\030\000\
\030\000\069\000\070\000\030\000\000\000\000\000\000\000\000\000\
\074\000\075\000\076\000\077\000\000\000\000\000\000\000\000\000\
\000\000\080\000\081\000\043\000\000\000\000\000\082\000\117\000\
\118\000\119\000\120\000\121\000\122\000\123\000\124\000\125\000\
\126\000\127\000\128\000\119\000\120\000\121\000\122\000\123\000\
\124\000\125\000\126\000\127\000\128\000"

let yycheck = "\032\000\
\085\000\001\000\035\000\062\000\001\001\002\001\022\000\040\000\
\029\000\042\000\021\001\001\001\002\001\029\000\030\000\090\000\
\049\000\001\001\002\001\040\001\041\000\029\000\038\000\056\000\
\045\001\041\000\085\000\112\000\040\001\062\000\063\000\047\000\
\044\001\041\000\038\001\040\001\111\000\053\000\040\001\055\000\
\045\001\041\001\058\000\045\001\041\001\040\001\046\001\044\001\
\064\000\046\001\045\001\041\001\085\000\112\000\044\001\088\000\
\046\001\041\001\143\000\021\001\044\001\042\001\046\001\040\001\
\001\001\002\001\082\000\100\000\045\001\007\001\008\001\073\000\
\041\001\053\000\090\000\108\000\135\000\046\001\058\000\112\000\
\082\000\040\001\038\001\054\000\143\000\044\001\171\000\172\000\
\059\000\091\000\092\000\093\000\094\000\018\001\034\001\111\000\
\030\001\043\001\131\000\101\000\042\001\186\000\038\001\105\000\
\041\001\040\001\037\001\044\001\041\001\046\001\143\000\040\001\
\171\000\172\000\046\001\117\000\118\000\119\000\120\000\121\000\
\122\000\123\000\124\000\125\000\126\000\127\000\128\000\186\000\
\005\001\006\001\007\001\008\001\043\001\042\001\015\001\016\001\
\017\001\018\001\171\000\172\000\001\001\002\001\003\001\004\001\
\005\001\006\001\007\001\008\001\009\001\010\001\011\001\012\001\
\033\001\186\000\040\001\042\001\041\001\038\001\001\001\002\001\
\040\001\015\001\016\001\017\001\018\001\167\000\168\000\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\041\001\041\001\040\001\038\001\044\001\
\038\001\046\001\001\001\002\001\003\001\004\001\005\001\006\001\
\001\001\002\001\009\001\010\001\011\001\012\001\041\001\001\001\
\002\001\044\001\040\001\046\001\040\001\001\001\002\001\041\001\
\043\001\014\001\044\001\044\001\046\001\001\001\002\001\003\001\
\004\001\005\001\006\001\042\001\040\001\009\001\010\001\011\001\
\012\001\041\001\041\001\044\001\041\001\044\001\041\001\046\001\
\041\001\041\001\046\001\044\001\014\001\046\001\042\001\041\001\
\024\001\042\001\044\001\043\001\046\001\041\001\041\001\044\001\
\044\001\043\001\046\001\044\001\044\001\041\001\043\001\042\001\
\044\001\043\001\046\001\001\001\002\001\003\001\004\001\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\012\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\001\001\002\001\003\001\004\001\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\012\001\043\001\
\000\000\000\000\130\000\042\001\019\001\020\001\044\001\022\001\
\023\001\043\001\025\001\026\001\027\001\028\001\029\001\043\001\
\031\001\032\001\044\001\041\001\035\001\036\001\037\001\041\001\
\043\001\040\001\043\001\043\001\041\001\041\001\044\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\001\001\002\001\003\001\004\001\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\012\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\010\000\255\255\019\001\020\001\041\001\
\022\001\023\001\255\255\025\001\026\001\027\001\028\001\029\001\
\255\255\031\001\032\001\041\001\255\255\035\001\036\001\037\001\
\255\255\255\255\040\001\255\255\255\255\255\255\255\255\041\001\
\001\001\002\001\003\001\004\001\005\001\006\001\007\001\008\001\
\009\001\010\001\011\001\012\001\006\001\255\255\255\255\255\255\
\255\255\255\255\255\255\013\001\255\255\255\255\255\255\255\255\
\255\255\019\001\020\001\255\255\255\255\255\255\255\255\255\255\
\026\001\027\001\028\001\029\001\255\255\255\255\255\255\255\255\
\041\001\035\001\036\001\037\001\006\001\255\255\040\001\255\255\
\255\255\255\255\044\001\013\001\255\255\015\001\016\001\017\001\
\018\001\019\001\020\001\255\255\255\255\255\255\255\255\255\255\
\026\001\027\001\028\001\029\001\255\255\255\255\006\001\255\255\
\255\255\035\001\036\001\037\001\038\001\013\001\040\001\255\255\
\255\255\255\255\255\255\019\001\020\001\255\255\255\255\255\255\
\255\255\255\255\026\001\027\001\028\001\029\001\255\255\255\255\
\255\255\255\255\255\255\035\001\036\001\037\001\019\001\020\001\
\040\001\022\001\023\001\255\255\025\001\026\001\027\001\028\001\
\029\001\255\255\031\001\032\001\255\255\255\255\035\001\036\001\
\037\001\019\001\020\001\040\001\255\255\255\255\255\255\255\255\
\026\001\027\001\028\001\029\001\255\255\255\255\255\255\255\255\
\255\255\035\001\036\001\037\001\255\255\255\255\040\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001"

let yynames_const = "\
  AND\000\
  OR\000\
  EQ\000\
  NEQ\000\
  PLUS\000\
  MINUS\000\
  MULTIPLY\000\
  DIVIDE\000\
  GRE\000\
  GEQ\000\
  LE\000\
  LEQ\000\
  NEG\000\
  ASSIGN\000\
  INT_KWORD\000\
  BOOL_KWORD\000\
  STRING_KWORD\000\
  VOID_KWORD\000\
  TRUE_KWORD\000\
  FALSE_KWORD\000\
  CLASS_KWORD\000\
  WHILE_KWORD\000\
  IF_KWORD\000\
  ELSE_KWORD\000\
  RETURN_KWORD\000\
  THIS_KWORD\000\
  SUPER_KWORD\000\
  NULL_KWORD\000\
  NEW_KWORD\000\
  MAIN_KWORD\000\
  READ_KWORD\000\
  PRINT_KWORD\000\
  PRIVATE_KWORD\000\
  EXTENDS_KWORD\000\
  NEWLINE\000\
  OPAREN\000\
  CPAREN\000\
  OBRACE\000\
  CBRACE\000\
  SEMICOLON\000\
  DOT\000\
  COMMA\000\
  COMMENT_LINE\000\
  COMMENT_OPEN\000\
  COMMENT_CLOSE\000\
  EOF\000\
  "

let yynames_block = "\
  INTEGER_LITERAL\000\
  STRING_LITERAL\000\
  VAR_IDENTIFIER\000\
  CLASS_IDENTIFIER\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'class_main) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'class_decl_list) in
    Obj.repr(
# 93 "mOOL_parser.mly"
                            ( (_1, _2) )
# 457 "mOOL_parser.ml"
               : MOOL_structs.mOOL_program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'method_main) in
    Obj.repr(
# 97 "mOOL_parser.mly"
                            ( ( _2, _4) )
# 465 "mOOL_parser.ml"
               : 'class_main))
; (fun __caml_parser_env ->
    Obj.repr(
# 101 "mOOL_parser.mly"
 ( None )
# 471 "mOOL_parser.ml"
               : 'inheritence))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 102 "mOOL_parser.mly"
                                  ( Some _2 )
# 478 "mOOL_parser.ml"
               : 'inheritence))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'inheritence) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'varmth_varmth_decl_list) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'method_decl_list) in
    Obj.repr(
# 109 "mOOL_parser.mly"
         ( (_2, _3,(fst _5), 
				(List.append (snd _5) _6)))
# 489 "mOOL_parser.ml"
               : 'class_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 114 "mOOL_parser.mly"
 ( [] )
# 495 "mOOL_parser.ml"
               : 'class_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'non_zero_class_decl_list) in
    Obj.repr(
# 115 "mOOL_parser.mly"
                            ( List.rev _1 )
# 502 "mOOL_parser.ml"
               : 'class_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'class_decl) in
    Obj.repr(
# 120 "mOOL_parser.mly"
                    ( [_1] )
# 509 "mOOL_parser.ml"
               : 'non_zero_class_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'non_zero_class_decl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'class_decl) in
    Obj.repr(
# 121 "mOOL_parser.mly"
                                       ( _2 :: _1)
# 517 "mOOL_parser.ml"
               : 'non_zero_class_decl_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 126 "mOOL_parser.mly"
 (  Public )
# 523 "mOOL_parser.ml"
               : 'modifier))
; (fun __caml_parser_env ->
    Obj.repr(
# 127 "mOOL_parser.mly"
                 ( Private )
# 529 "mOOL_parser.ml"
               : 'modifier))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'mthd_param_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'var_decl_stmt_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'non_zero_stmt_list) in
    Obj.repr(
# 137 "mOOL_parser.mly"
   ( { 
				modifier = Public;
				rettype = VoidT; 
				mOOLid = SimpleVarId "main"; 
				ir3id = (SimpleVarId "main"); 
				params = _4; 
				localvars = _7; stmts = _8;
			})
# 545 "mOOL_parser.ml"
               : 'method_main))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 148 "mOOL_parser.mly"
                (  SimpleVarId _1 )
# 552 "mOOL_parser.ml"
               : 'var_id_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : 'type_KWORD) in
    let _2 = (Parsing.peek_val __caml_parser_env 7 : 'var_id_rule) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'mthd_param_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'var_decl_stmt_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'non_zero_stmt_list) in
    Obj.repr(
# 158 "mOOL_parser.mly"
   ( { 
				modifier=Public;
				rettype=_1; 
				mOOLid=_2;
				ir3id= _2;				
				params=_4; 
				localvars=_7; stmts=_8;
			})
# 570 "mOOL_parser.ml"
               : 'method_decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 9 : 'modifier) in
    let _2 = (Parsing.peek_val __caml_parser_env 8 : 'type_KWORD) in
    let _3 = (Parsing.peek_val __caml_parser_env 7 : 'var_id_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : 'mthd_param_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'var_decl_stmt_list) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'non_zero_stmt_list) in
    Obj.repr(
# 174 "mOOL_parser.mly"
   ( { 
				modifier=_1;
				rettype=_2; 
				mOOLid=_3;
				ir3id= _3;				
				params=_5; 
				localvars=_8; stmts=_9;
			})
# 589 "mOOL_parser.ml"
               : 'method_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 185 "mOOL_parser.mly"
 ( [] )
# 595 "mOOL_parser.ml"
               : 'method_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'non_zero_method_decl_list) in
    Obj.repr(
# 186 "mOOL_parser.mly"
                             ( List.rev _1 )
# 602 "mOOL_parser.ml"
               : 'method_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'method_decl) in
    Obj.repr(
# 190 "mOOL_parser.mly"
                    ( [_1] )
# 609 "mOOL_parser.ml"
               : 'non_zero_method_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'non_zero_method_decl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'method_decl) in
    Obj.repr(
# 191 "mOOL_parser.mly"
                                         ( _2 :: _1)
# 617 "mOOL_parser.ml"
               : 'non_zero_method_decl_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 195 "mOOL_parser.mly"
              ( BoolT )
# 623 "mOOL_parser.ml"
               : 'type_KWORD))
; (fun __caml_parser_env ->
    Obj.repr(
# 196 "mOOL_parser.mly"
                 ( IntT )
# 629 "mOOL_parser.ml"
               : 'type_KWORD))
; (fun __caml_parser_env ->
    Obj.repr(
# 197 "mOOL_parser.mly"
                 ( StringT )
# 635 "mOOL_parser.ml"
               : 'type_KWORD))
; (fun __caml_parser_env ->
    Obj.repr(
# 198 "mOOL_parser.mly"
               ( VoidT )
# 641 "mOOL_parser.ml"
               : 'type_KWORD))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 199 "mOOL_parser.mly"
                    (  ObjectT _1 )
# 648 "mOOL_parser.ml"
               : 'type_KWORD))
; (fun __caml_parser_env ->
    Obj.repr(
# 204 "mOOL_parser.mly"
 ( [] )
# 654 "mOOL_parser.ml"
               : 'mthd_param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'non_zero_mthd_param_list) in
    Obj.repr(
# 205 "mOOL_parser.mly"
                            ( List.rev _1 )
# 661 "mOOL_parser.ml"
               : 'mthd_param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'type_KWORD) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'var_id_rule) in
    Obj.repr(
# 210 "mOOL_parser.mly"
  ( [(_1, _2)] )
# 669 "mOOL_parser.ml"
               : 'non_zero_mthd_param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'non_zero_mthd_param_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'type_KWORD) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'var_id_rule) in
    Obj.repr(
# 212 "mOOL_parser.mly"
  ( (_3, _4) :: _1)
# 678 "mOOL_parser.ml"
               : 'non_zero_mthd_param_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 218 "mOOL_parser.mly"
 ( [] )
# 684 "mOOL_parser.ml"
               : 'var_decl_stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'non_zero_var_decl_stmt_list) in
    Obj.repr(
# 219 "mOOL_parser.mly"
                               ( List.rev _1 )
# 691 "mOOL_parser.ml"
               : 'var_decl_stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'type_KWORD) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'var_id_rule) in
    Obj.repr(
# 224 "mOOL_parser.mly"
  ( [(_1, _2)] )
# 699 "mOOL_parser.ml"
               : 'non_zero_var_decl_stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'non_zero_var_decl_stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'type_KWORD) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'var_id_rule) in
    Obj.repr(
# 226 "mOOL_parser.mly"
  ( (_2, _3) :: _1)
# 708 "mOOL_parser.ml"
               : 'non_zero_var_decl_stmt_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 232 "mOOL_parser.mly"
 ( ([],[]) )
# 714 "mOOL_parser.ml"
               : 'varmth_varmth_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'non_zero_varmth_decl_list) in
    Obj.repr(
# 234 "mOOL_parser.mly"
  ( ((fst _1),(snd _1)) )
# 721 "mOOL_parser.ml"
               : 'varmth_varmth_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'type_KWORD) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'var_id_rule) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'non_zero_varmth_decl_list) in
    Obj.repr(
# 239 "mOOL_parser.mly"
   ( (((Public,(_1, _2)) :: (fst _4)), (snd _4)))
# 730 "mOOL_parser.ml"
               : 'non_zero_varmth_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'modifier) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'type_KWORD) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'var_id_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'non_zero_varmth_decl_list) in
    Obj.repr(
# 241 "mOOL_parser.mly"
   ( (((_1,(_2, _3)) :: (fst _5)), (snd _5)))
# 740 "mOOL_parser.ml"
               : 'non_zero_varmth_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'method_decl) in
    Obj.repr(
# 243 "mOOL_parser.mly"
   ( ([], [_1]))
# 747 "mOOL_parser.ml"
               : 'non_zero_varmth_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'type_KWORD) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'var_id_rule) in
    Obj.repr(
# 245 "mOOL_parser.mly"
   ( ([(Public,(_1, _2))],[]) )
# 755 "mOOL_parser.ml"
               : 'non_zero_varmth_decl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 250 "mOOL_parser.mly"
                                ( ReturnStmt _2 )
# 762 "mOOL_parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 251 "mOOL_parser.mly"
                             ( ReturnVoidStmt)
# 768 "mOOL_parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'exp) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'non_zero_stmt_list) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'non_zero_stmt_list) in
    Obj.repr(
# 254 "mOOL_parser.mly"
                                               ( IfStmt (_3,_6,_10) )
# 777 "mOOL_parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'exp) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'non_zero_stmt_list) in
    Obj.repr(
# 256 "mOOL_parser.mly"
                                    ( WhileStmt (_3,_6) )
# 785 "mOOL_parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'var_id_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 257 "mOOL_parser.mly"
                                     ( AssignStmt (_1, _3) )
# 793 "mOOL_parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'var_id_rule) in
    Obj.repr(
# 259 "mOOL_parser.mly"
                                       ( ReadStmt (_3) )
# 800 "mOOL_parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    Obj.repr(
# 261 "mOOL_parser.mly"
                                ( PrintStmt (_3) )
# 807 "mOOL_parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'atom) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'var_id_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 263 "mOOL_parser.mly"
   ( AssignFieldStmt ( FieldAccess ( _1, _3), _5) )
# 816 "mOOL_parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'atom) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'exp_list) in
    Obj.repr(
# 264 "mOOL_parser.mly"
                                         ( MdCallStmt (MdCall ( _1, _3)) )
# 824 "mOOL_parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 268 "mOOL_parser.mly"
 ( [] )
# 830 "mOOL_parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'non_zero_stmt_list) in
    Obj.repr(
# 269 "mOOL_parser.mly"
                      ( _1 )
# 837 "mOOL_parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 273 "mOOL_parser.mly"
            ( [_1] )
# 844 "mOOL_parser.ml"
               : 'non_zero_stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'non_zero_stmt_list) in
    Obj.repr(
# 274 "mOOL_parser.mly"
                             ( _1 :: _2)
# 852 "mOOL_parser.ml"
               : 'non_zero_stmt_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 279 "mOOL_parser.mly"
                                ( Some _2 )
# 859 "mOOL_parser.ml"
               : 'cast))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 283 "mOOL_parser.mly"
            ( BinaryExp (BooleanOp "||", _1, _3) )
# 867 "mOOL_parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 284 "mOOL_parser.mly"
                ( BinaryExp (BooleanOp "&&", _1, _3) )
# 875 "mOOL_parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 285 "mOOL_parser.mly"
               ( BinaryExp (RelationalOp "==", _1, _3) )
# 883 "mOOL_parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 286 "mOOL_parser.mly"
               ( BinaryExp (RelationalOp "!=", _1, _3) )
# 891 "mOOL_parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 287 "mOOL_parser.mly"
               ( BinaryExp (RelationalOp ">", _1, _3) )
# 899 "mOOL_parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 288 "mOOL_parser.mly"
               ( BinaryExp (RelationalOp ">=", _1, _3) )
# 907 "mOOL_parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 289 "mOOL_parser.mly"
               ( BinaryExp (RelationalOp "<", _1, _3) )
# 915 "mOOL_parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 290 "mOOL_parser.mly"
               ( BinaryExp (RelationalOp "<=", _1, _3) )
# 923 "mOOL_parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 291 "mOOL_parser.mly"
                 ( BinaryExp (AritmeticOp "+",_1,_3) )
# 931 "mOOL_parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 292 "mOOL_parser.mly"
                  ( BinaryExp (AritmeticOp "-",_1,_3) )
# 939 "mOOL_parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 293 "mOOL_parser.mly"
                    ( BinaryExp (AritmeticOp "*",_1,_3) )
# 947 "mOOL_parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 294 "mOOL_parser.mly"
                   ( BinaryExp (AritmeticOp "+",_1,_3) )
# 955 "mOOL_parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 295 "mOOL_parser.mly"
                                ( UnaryExp (UnaryOp "!", _2) )
# 962 "mOOL_parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 296 "mOOL_parser.mly"
                       ( UnaryExp (UnaryOp "-", _2) )
# 969 "mOOL_parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 297 "mOOL_parser.mly"
           ( _1)
# 976 "mOOL_parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atom) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'var_id_rule) in
    Obj.repr(
# 301 "mOOL_parser.mly"
                        ( FieldAccess ( _1, _3) )
# 984 "mOOL_parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 302 "mOOL_parser.mly"
                  ( StringLiteral ( _1 ) )
# 991 "mOOL_parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 303 "mOOL_parser.mly"
                   ( IntLiteral ( _1 ) )
# 998 "mOOL_parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 304 "mOOL_parser.mly"
               ( BoolLiteral (true) )
# 1004 "mOOL_parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 305 "mOOL_parser.mly"
                ( BoolLiteral (false) )
# 1010 "mOOL_parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'atom) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exp_list) in
    Obj.repr(
# 306 "mOOL_parser.mly"
                                ( MdCall ( _1, _3) )
# 1018 "mOOL_parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 307 "mOOL_parser.mly"
                   ( ThisWord )
# 1024 "mOOL_parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 308 "mOOL_parser.mly"
                                         ( SuperWord)
# 1030 "mOOL_parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 309 "mOOL_parser.mly"
                 ( NullWord )
# 1036 "mOOL_parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'var_id_rule) in
    Obj.repr(
# 310 "mOOL_parser.mly"
                   ( Var _1 )
# 1043 "mOOL_parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 311 "mOOL_parser.mly"
                                            ( ObjectCreate _2 )
# 1050 "mOOL_parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'type_KWORD) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 312 "mOOL_parser.mly"
                                 ( CastExp ( _4, _2) )
# 1058 "mOOL_parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 313 "mOOL_parser.mly"
                      ( _2 )
# 1065 "mOOL_parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 317 "mOOL_parser.mly"
 ( [] )
# 1071 "mOOL_parser.ml"
               : 'exp_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'non_zero_exp_list) in
    Obj.repr(
# 318 "mOOL_parser.mly"
                     ( List.rev _1 )
# 1078 "mOOL_parser.ml"
               : 'exp_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 322 "mOOL_parser.mly"
           ( [_1] )
# 1085 "mOOL_parser.ml"
               : 'non_zero_exp_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'non_zero_exp_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 323 "mOOL_parser.mly"
                                ( _3 :: _1)
# 1093 "mOOL_parser.ml"
               : 'non_zero_exp_list))
(* Entry input *)
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
let input (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : MOOL_structs.mOOL_program)
;;
# 326 "mOOL_parser.mly"

# 1120 "mOOL_parser.ml"
