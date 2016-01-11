
/* ===================================================== */
/* ============== CS4212 Compiler Design ============== *)
(* 		  Parsing of MOOL programs into  Asts			 *)
(* ===================================================== *)

/* ============ Error reporting Code ============ */
%{

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
  
%}

/* =========== Definitions of tokens =========== */

%token AND OR 
%token EQ NEQ 
%token PLUS MINUS 
%token MULTIPLY DIVIDE
%token GRE GEQ 
%token LE LEQ 
%token NEG
%token ASSIGN

%LEFT AND OR
%NONASSOC EQ NEQ GRE GEQ LE LEQ 
%LEFT PLUS MINUS
%LEFT MULTIPLY DIVIDE
%RIGHT NEG

%token INT_KWORD 
%token BOOL_KWORD STRING_KWORD 
%token VOID_KWORD

%token TRUE_KWORD FALSE_KWORD
%token CLASS_KWORD 

%token WHILE_KWORD 
%token IF_KWORD ELSE_KWORD
%token RETURN_KWORD
%token THIS_KWORD 
%token SUPER_KWORD
%token NULL_KWORD
%token NEW_KWORD
%token MAIN_KWORD
%token READ_KWORD
%token PRINT_KWORD
%token PRIVATE_KWORD
%token EXTENDS_KWORD

%token <int> INTEGER_LITERAL
%token <string> STRING_LITERAL

%token <string> VAR_IDENTIFIER
%token <string> CLASS_IDENTIFIER
 
%token NEWLINE

%token OPAREN CPAREN 
%token OBRACE CBRACE

%token SEMICOLON
%token DOT
%token COMMA

%token COMMENT_LINE 
%token COMMENT_OPEN COMMENT_CLOSE 

%token EOF

/* ============= Return type of parser ========== */
%start input
%type <MOOL_structs.mOOL_program> input
%% 

/* ========= Grammar rules and actions ========== */

input:    
	class_main class_decl_list { ($1, $2) }

class_main:
	CLASS_KWORD CLASS_IDENTIFIER 
		OBRACE method_main CBRACE { ( $2, $4) }
;

inheritence:
	{ None }
	| EXTENDS_KWORD CLASS_IDENTIFIER { Some $2 }
;

class_decl:
	CLASS_KWORD CLASS_IDENTIFIER inheritence 
		OBRACE 
			varmth_varmth_decl_list  method_decl_list
		CBRACE { ($2, $3,(fst $5), 
				(List.append (snd $5) $6))}
;
	
class_decl_list: 
	{ [] }
	| non_zero_class_decl_list { List.rev $1 }
;


non_zero_class_decl_list: 
	class_decl 								{ [$1] }
	| non_zero_class_decl_list class_decl	{ $2 :: $1}
;


modifier:
	{  Public }
	| PRIVATE_KWORD { Private }
;

method_main:
		VOID_KWORD MAIN_KWORD
		OPAREN mthd_param_list CPAREN 
		OBRACE 
		var_decl_stmt_list 
		non_zero_stmt_list 
		CBRACE 
			{ { 
				modifier = Public;
				rettype = VoidT; 
				mOOLid = SimpleVarId "main"; 
				ir3id = (SimpleVarId "main"); 
				params = $4; 
				localvars = $7; stmts = $8;
			}}
;

var_id_rule:
	VAR_IDENTIFIER {  SimpleVarId $1 }
;

method_decl:
		type_KWORD var_id_rule
		OPAREN mthd_param_list CPAREN 
		OBRACE 
		var_decl_stmt_list 
		non_zero_stmt_list 
		CBRACE 
			{ { 
				modifier=Public;
				rettype=$1; 
				mOOLid=$2;
				ir3id= $2;				
				params=$4; 
				localvars=$7; stmts=$8;
			}}
		|
	    modifier
	   	type_KWORD var_id_rule
		OPAREN mthd_param_list CPAREN 
		OBRACE 
		var_decl_stmt_list 
		non_zero_stmt_list 
		CBRACE 
			{ { 
				modifier=$1;
				rettype=$2; 
				mOOLid=$3;
				ir3id= $3;				
				params=$5; 
				localvars=$8; stmts=$9;
			}}
;	

method_decl_list: 
	{ [] }
	| non_zero_method_decl_list { List.rev $1 }
;

non_zero_method_decl_list: 
	method_decl 							{ [$1] }
	| non_zero_method_decl_list method_decl	{ $2 :: $1}
;

type_KWORD: 
	BOOL_KWORD 		{ BoolT }
    | INT_KWORD 	{ IntT }
	| STRING_KWORD 	{ StringT }
	| VOID_KWORD 	{ VoidT }
	| CLASS_IDENTIFIER {  ObjectT $1 }
;

/* === Rule for defining the list of parameters of a method === */
mthd_param_list:
	{ [] }
	| non_zero_mthd_param_list { List.rev $1 }
;

non_zero_mthd_param_list: 
	type_KWORD var_id_rule 									
		{ [($1, $2)] }
	| non_zero_mthd_param_list COMMA type_KWORD var_id_rule	
		{ ($3, $4) :: $1}
;		


/* === Rules for defining the list of variable declarations in the body of a method === */
var_decl_stmt_list:
	{ [] }
	| non_zero_var_decl_stmt_list { List.rev $1 }
;

non_zero_var_decl_stmt_list: 
	type_KWORD var_id_rule SEMICOLON		
		{ [($1, $2)] }
	| non_zero_var_decl_stmt_list type_KWORD var_id_rule SEMICOLON 
		{ ($2, $3) :: $1}
;


/* === Rule for defining the list of field declarations in a class and the first method declaration ===*/
varmth_varmth_decl_list:
	{ ([],[]) }
	| non_zero_varmth_decl_list 
		{ ((fst $1),(snd $1)) }
;

non_zero_varmth_decl_list: 
	type_KWORD var_id_rule SEMICOLON non_zero_varmth_decl_list
			{ (((Public,($1, $2)) :: (fst $4)), (snd $4))}
	| modifier type_KWORD var_id_rule SEMICOLON non_zero_varmth_decl_list
			{ ((($1,($2, $3)) :: (fst $5)), (snd $5))}
	| method_decl 
			{ ([], [$1])}
	| type_KWORD var_id_rule SEMICOLON		
			{ ([(Public,($1, $2))],[]) }
;

/* === Rule for defining the different types of statements in a method body ===*/
stmt: 
	RETURN_KWORD exp SEMICOLON 				{ ReturnStmt $2 }
	| RETURN_KWORD SEMICOLON				{ ReturnVoidStmt}
	| IF_KWORD OPAREN exp CPAREN 
		OBRACE non_zero_stmt_list CBRACE 
		ELSE_KWORD OBRACE non_zero_stmt_list CBRACE 	{ IfStmt ($3,$6,$10) } 
	| WHILE_KWORD OPAREN exp CPAREN 
		OBRACE non_zero_stmt_list CBRACE 	{ WhileStmt ($3,$6) } 
	| var_id_rule ASSIGN exp SEMICOLON 	{ AssignStmt ($1, $3) }
	| READ_KWORD 
		OPAREN var_id_rule CPAREN SEMICOLON 	{ ReadStmt ($3) }
	| PRINT_KWORD 
		OPAREN exp CPAREN SEMICOLON 		{ PrintStmt ($3) }
	| atom DOT var_id_rule ASSIGN exp SEMICOLON 
			{ AssignFieldStmt ( FieldAccess ( $1, $3), $5) }
	| atom OPAREN exp_list CPAREN SEMICOLON	{ MdCallStmt (MdCall ( $1, $3)) }
;

stmt_list: 
	{ [] }
	| non_zero_stmt_list { $1 }
;

non_zero_stmt_list: 
	stmt 						{ [$1] }
	| stmt non_zero_stmt_list  	{ $1 :: $2}
;

/* === Rule for defining the different types of expressions in a method body */
cast:
	OPAREN CLASS_IDENTIFIER CPAREN { Some $2 }
;

exp: 
	exp OR exp	{ BinaryExp (BooleanOp "||", $1, $3) }
	| exp AND exp 	{ BinaryExp (BooleanOp "&&", $1, $3) }
	| exp EQ exp 	{ BinaryExp (RelationalOp "==", $1, $3) }
	| exp NEQ exp { BinaryExp (RelationalOp "!=", $1, $3) }
	| exp GRE exp { BinaryExp (RelationalOp ">", $1, $3) }
	| exp GEQ exp { BinaryExp (RelationalOp ">=", $1, $3) }
	| exp LE exp 	{ BinaryExp (RelationalOp "<", $1, $3) }
	| exp LEQ exp { BinaryExp (RelationalOp "<=", $1, $3) }
	| exp PLUS exp		{ BinaryExp (AritmeticOp "+",$1,$3) }
	| exp MINUS exp 	{ BinaryExp (AritmeticOp "-",$1,$3) }
	| exp MULTIPLY exp	{ BinaryExp (AritmeticOp "*",$1,$3) }
	| exp DIVIDE exp 	{ BinaryExp (AritmeticOp "+",$1,$3) }
    | NEG exp %prec NEG     { UnaryExp (UnaryOp "!", $2) }
	| MINUS exp %prec NEG	{ UnaryExp (UnaryOp "-", $2) }
	| atom 			{ $1}
;

atom:
	atom DOT var_id_rule 		{ FieldAccess ( $1, $3) }
	| STRING_LITERAL	{ StringLiteral ( $1 ) }
	| INTEGER_LITERAL	{ IntLiteral ( $1 ) }
	| TRUE_KWORD 	{ BoolLiteral (true) }
	| FALSE_KWORD 	{ BoolLiteral (false) }
	| atom OPAREN exp_list CPAREN 	{ MdCall ( $1, $3) }
	| THIS_KWORD 					{ ThisWord }
	| SUPER_KWORD                           { SuperWord }
	| NULL_KWORD				{ NullWord }
	| var_id_rule 				{ Var $1 }
	| NEW_KWORD CLASS_IDENTIFIER OPAREN CPAREN { ObjectCreate $2 }
	| OPAREN type_KWORD CPAREN atom { CastExp ( $4, $2) }	
	| OPAREN exp CPAREN 	{ $2 }
;

exp_list: 
	{ [] }
	| non_zero_exp_list { List.rev $1 }
;

non_zero_exp_list: 
	exp 						{ [$1] }
	| non_zero_exp_list COMMA exp		{ $3 :: $1}
;
	
%%
