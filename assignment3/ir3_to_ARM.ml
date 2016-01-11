(* ===================================================== *)
(* ============== CS4212 Compiler Design =============== *)
(*         Transformation IR3 to ARM machine code        *)
(* ==================== TEAM MOANG ===================== *)
(* ===================================================== *)

open MOOL_structs
open Ir3mOOL_structs
open Arm_structs

let labelcount = ref 0 
let fresh_label () = 
	(labelcount:=!labelcount+1; !labelcount)

(*  *)
type stmt_node =
{
	id: int;
	stmt: ir3_stmt;
	visited: bool;
	succ: int list;
	pred: int list;
	(*def: "NEED TO DEFINED A SET";
	use: "NEED TO DEFINED A SET";
	live_in: "NEED TO DEFINED A SET";
	live_out: "NEED TO DEFINED A SET";*)
}

(* hash table of the statements within a method *)
type stmtHT = (int, stmt_node) Hashtbl.t

(* value of method in mdHT *)
type md_structure =
{
	md_ir3: md_decl3;
	stmts: stmtHT
}

(* methods hash tables. string is the name of the method *)
(* Can we do new mdHT???? *)
type mdHT = (string, md_structure) Hashtbl.t


(* {
	id: label3;
	localvars3:(var_decl3 list);
	ir3stmts:(ir3_stmt list) 
}
 *)
(*Scan a method and return a list of blocks*)
(*let get_blocks_from_mth main_md_decl =
	let localvars = main_md_decl.localvars @ main_md_decl.params in
		let helper statement_list block_list = 
			match stmt_list with
			head :: tail ->
				(match head with
				Label3 lbl3 -> (*Add previous block to list, create new block, add statement*)
				| IfStmt3 exp, lbl3 -> (*Add stmt to block *)
				| GoTo3 lbl3 -> (*Add previous block to list, create new block, add statement*)
				| ReadStmt3 id3 -> (*Add stmt to block*)
				| PrintStmt3 idc3 -> (*Add stmt to block*)
				| AssignStmt3 id3, ir3_exp -> (*Add stmt to block*)
				| AssignDeclStmt3 ir3_type, id3, ir3_exp -> (*Add stmt to block*)
				| AssignFieldStmt3 ir3_exp, ir3_exp ->  (*Add stmt to block*)
				| MdCallStmt3 ir3_exp -> (*Add stmt to block*)
				| ReturnStmt3 id3 -> (*Add stmt to block*)
				| ReturnVoidStmt3 -> (*Add stmt to block*)
			[] -> block_list (*Return list with blocks*)
		in helper main_md_decl.stmt_list [] 
*)

(*
let create_blocks ir3prog = 
	let (main_md_decl, md_list) = ir3prog in
		(*Get blocks from main method first*)
		let main_mth_blocks = get_blocks_from_mth main_md_decl in
			"Got all blocks in main_method"*)

(* Generate data and machine instruction *)
let generate_instructions
	(md:md_decl3):arm_program * arm_program =
	[], []
	(* Blabla *)

(* entry point function *)
let ir3_program_to_ARM (ir3prog:ir3_program):arm_program =
	let (cdata,main_md,md_list) = ir3prog in
	let mds = main_md :: md_list in
	let (data_instructions,machine_instructions) =
		let rec helper di mi methods =
			match methods with
			| head :: tail ->
				let (md_di,md_mi) = generate_instructions head in
				let (di_new,mi_new) = (di@md_di,mi@md_mi) in
				helper di_new mi_new tail
			| [] -> (di,mi)
		in
		helper [] [] mds in
	PseudoInstr ".data" :: 
	data_instructions @
	PseudoInstr ".text" :: 
	PseudoInstr ".global main" :: 
	machine_instructions