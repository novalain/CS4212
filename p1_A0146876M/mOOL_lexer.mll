(* ===================================================== *)
(* ============== CS4212 Compiler Design ============== *)
(* 			  	 Lexing of MOOL programs 			 	 *)
(* ===================================================== *)

{
  open MOOL_parser (* Assumes the parser file is "parser.mly". *)
  let incr_linenum file_name lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
      lexbuf.Lexing.lex_curr_p <- 
	{ pos with
	    Lexing.pos_fname = file_name;
	    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
	    Lexing.pos_bol = pos.Lexing.pos_cnum;
	}
}

let digit = ['0'-'9']
let varid = ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let classid = ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let charhex = ['0'-'9' 'A'-'F' 'a'-'f']
let whitespace = [' ' '\t']
let newline = ('\n' | '\r' | "\r\n")
let charprintable = ['\032' - '\126']

(* Escaped character followed by anything, or whatever character
   that does not include doublequotes or backslash *)
let stringliteral = ("\\"charprintable | ['\032' - '\033' '\035' - '\091' '\093' - '\126'])*

rule token file_name = parse
  | "||" { LOGICAL_OR }
  | "&&" { LOGICAL_AND }
  | "<=" { LESS_EQUAL }
  | ">=" { GREATER_EQUAL }
  | "==" { EQUAL }
  | "!=" { INEQUAL }
  | '<' { LESS }
  | '>' { GREATER }
  | '=' { ASSIGN }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { MULTIPLY }
  | '/' { DIVIDE }
  | '!' { NAGATE }
  | '{'		{ OBRACE }
  | '}'		{ CBRACE }
  | '('		{ OPAREN }
  | ')'		{ CPAREN }
  | "super" { SUPER_KWORD }
  | "private" { PRIVATE_KWORD }
  | "extends" { EXTENDS_KWORD }
  | "new" { NEW_KWORD }
  | "Int"	{ INT_KWORD}
  | "Bool"	{ BOOL_KWORD}
  | "String" { STRING_KWORD}
  | "Void"  { VOID_KWORD }
  | "true"	{ TRUE_KWORD }
  | "false"	{ FALSE_KWORD }
  | "class" { CLASS_KWORD }
  | "while"	{ WHILE_KWORD }
  | "if" 	{ IF_KWORD }
  | "else"	{ ELSE_KWORD }
  | "return" { RETURN_KWORD }
  | "this"	{ THIS_KWORD }
  | "NULL" { NULL_KWORD }
  | "main"  { MAIN_KWORD }
  | "readln" { READ_KWORD }
  | "println" { PRINT_KWORD }
  | "//" {comment_single file_name lexbuf} 
  | "/*" {comment_multi 0 file_name lexbuf} 
  | ';'		{ SEMICOLON }
  | '.'		{ DOT }
  | ','		{ COMMA }
  | classid as str		{ CLASS_IDENTIFIER str}
  | varid as str1		{ VAR_IDENTIFIER str1 }
  | digit+ as num
		{ INTEGER_LITERAL (int_of_string num) }
  |  "\""(stringliteral
          as s)
	"\"" { STRING_LITERAL s }
  | whitespace { token file_name lexbuf }
  | '\n' { incr_linenum file_name lexbuf; token file_name lexbuf }
  | '\r' { incr_linenum file_name lexbuf; token file_name lexbuf }
  | "\r\n" { incr_linenum file_name lexbuf; token file_name lexbuf }
  | eof		{ EOF }

(* The comments make their own buffer that calls itself recursively
   until a closing tag has been found on nested level n = 0 *)
and comment_multi n file_name = parse
  | "*/" {
    if n = 0 then token file_name lexbuf
    else comment_multi (n-1) file_name lexbuf
  }
  | "/*" { 
  comment_multi (n+1) file_name lexbuf
  }
  | _ { comment_multi n file_name lexbuf }
  | eof { print_endline "multi-comments are not closed"; EOF }

and comment_single file_name = parse
  | newline {token file_name lexbuf}
  | _ {comment_single file_name lexbuf}
  | eof { print_endline "single-comments are not closed!"; EOF}

  
