
(* ===================================================== *)
(* ============== CS4212 Compiler Design ============== *)
(*   Transformation to intermediary representation IR3   *)
(* ===================================================== *)

open MOOL_structs
open Ir3mOOL_structs

let labelcount = ref 0 
let fresh_label () = 
	(labelcount:=!labelcount+1; !labelcount)

let varcount = ref 0 
let fresh_var () = 
	(varcount:=!varcount+1; (string_of_int !varcount))

let mOOL_var_decl_list_to_IR3 var_decl_list =
  let p v = ( (fst v), string_of_var_id(snd v) ) in
  List.map p var_decl_list

let iR3Expr_to_id3
    (ir3exp: ir3_exp) (t: mOOL_type) (l1: var_decl3 list) (l2: ir3_stmt list) (toid3 : bool) : (ir3_exp * var_decl3 list * ir3_stmt list) =
  if (toid3) then
    begin
      let new_varname = "_t" ^ fresh_var() in
      match ir3exp with
      | BinaryExp3 (op, idc3_1, idc3_2) ->
        (let exp_return = Idc3Expr (Var3 new_varname) in
        let exp_stmt = BinaryExp3 (op, idc3_1, idc3_2) in
        (exp_return, l1 @ [(t, new_varname)], l2 @ [(AssignStmt3 (new_varname, exp_stmt))]))
      | UnaryExp3 (op, idc3_1) ->
        (let exp_return = Idc3Expr (Var3 new_varname) in
        let exp_stmt = UnaryExp3 (op, idc3_1) in
        (exp_return, l1 @ [(t, new_varname)], l2 @ [(AssignStmt3 (new_varname, exp_stmt))]))
      | FieldAccess3 (id3_1, id3_2) ->
         (let exp_return = Idc3Expr (Var3 new_varname) in
          let exp_stmt = FieldAccess3 (id3_1, id3_2) in
          (exp_return, l1 @ [(t, new_varname)], l2 @ [(AssignStmt3 (new_varname, exp_stmt))]))
      | MdCall3 (caller, id3_1, idc3_list) ->
          (let exp_return = Idc3Expr (Var3 new_varname) in
          let exp_stmt = MdCall3 (caller, id3_1, idc3_list) in
          (exp_return, l1 @ [(t, new_varname)], l2 @ [(AssignStmt3 (new_varname, exp_stmt))]))
      | _ -> failwith "Error in iR3Expr_to_id3"
    end
  else (ir3exp, l1, l2)

let iR3Expr_get_idc3 (exp: ir3_exp) : idc3 = 
  match exp with 
  Idc3Expr e -> e
  | _ -> failwith("IDC3 Error, could not get IDC3 from IR3")

let iR3Expr_get_id3 (exp: ir3_exp) = 
  match iR3Expr_get_idc3 exp with
  | Var3 m_id -> m_id
  | IntLiteral3 m_id -> (string_of_int m_id)
  | StringLiteral3 m_id -> m_id
  | BoolLiteral3 m_id -> (string_of_bool m_id)

let mOOLvarid_to_IR3Expr (classid: class_name) (v:var_id) (toid3:bool) :(ir3_exp * var_decl3 list * ir3_stmt list) =
  match v with
  SimpleVarId id -> (Idc3Expr (Var3 id),[],[])
  | TypedVarId (id,t,s) -> if (s == 1) (* class scope *)
      then let thisExpr = FieldAccess3 ("this",id) in
        (iR3Expr_to_id3 thisExpr t [] [] toid3)
      else let newExpr = Idc3Expr (Var3 id) in (newExpr,[], [])

let rec mOOLexpr_to_IR3  (cname: class_name)
(m_exp:mOOL_exp) (toidc3:bool) (toid3:bool) : (ir3_exp * var_decl3 list * ir3_stmt list) =
  let rec helper (me:mOOL_exp) (toidc3:bool) (toid3:bool) =
    match me with
    TypedExp (te, t) ->
        (match te with
        UnaryExp(op, exp) -> 
            let (argIR3, vars, stmts) = (helper exp true false) in
            let argIdc3 = (iR3Expr_get_idc3 argIR3) in
            let newExpr = UnaryExp3 (op, argIdc3) in
            (iR3Expr_to_id3 newExpr t vars stmts toidc3)
        | BinaryExp(op, arg1, arg2) -> (* From slides *)
            let (arg1IR3,vars1,stmts1) = (helper arg1 true false) in
            let (arg2IR3,vars2,stmts2) = (helper arg2 true false) in
            let arg1Idc3 = (iR3Expr_get_idc3 arg1IR3) in
            let arg2Idc3 = (iR3Expr_get_idc3 arg2IR3) in
            let newExpr = BinaryExp3 (op, arg1Idc3, arg2Idc3) in
            (iR3Expr_to_id3 newExpr t (vars1@vars2) (stmts1@stmts2) toidc3)
        | FieldAccess(exp, var_id) -> 
            let (expIR3, vars, stmts) = (helper exp true true) in
            let expId3 = iR3Expr_get_id3 expIR3 in
            let newExpr = FieldAccess3 (expId3, string_of_var_id var_id) in
            (iR3Expr_to_id3 newExpr t vars stmts toidc3)
        | ObjectCreate(cname) -> (ObjectCreate3(cname),[],[])
        | MdCall (m_exp, exp_list) -> ((ObjectCreate3 "this"), [], [])
        | BoolLiteral (m_bool) -> (Idc3Expr(BoolLiteral3 m_bool) , [],[])
        | IntLiteral (m_int) -> (Idc3Expr(IntLiteral3 m_int) , [],[])
        | StringLiteral (m_string)-> (Idc3Expr(StringLiteral3 m_string) , [],[])
        | ThisWord ->  ((ObjectCreate3 "this"), [], [])
        | NullWord ->  ((ObjectCreate3 "null"), [], [])
        | SuperWord ->  ((ObjectCreate3 "this"), [], [])
        | Var (var_id) -> (mOOLvarid_to_IR3Expr cname var_id toidc3)(*(mOOLvarid_to_IR3Expr cname var_id toidc3)*)
        | CastExp(m_exp, m_type) -> ((ObjectCreate3 "this"), [], [])
        | _ -> failwith("Big Error: Expression is untyped"))
    | IntLiteral v ->
        let newExpr = Idc3Expr (IntLiteral3 v) in 
        (newExpr, [],[])
    | BoolLiteral v ->
        let newExpr = Idc3Expr (BoolLiteral3 v) in (newExpr, [],[])
    | StringLiteral v ->
        let newExpr = Idc3Expr (StringLiteral3 v) in (newExpr, [],[])    
    | _ -> failwith("IR3 Error: Expression " ^ string_of_mOOL_expr m_exp ^ " is not typed correctly ")
  in helper m_exp toidc3 toid3
    
let rec mstmts_to_IR3stmts cname curr_method method_statements : var_decl3 list * ir3_stmt list =
    match method_statements with 
    head :: tail ->  
      let rec helper statements = 
        match statements with 
        IfStmt (exp, stmtlist1, stmtlist2) -> 
            let (expr3, exprvars, exprstmts) =
              (mOOLexpr_to_IR3 cname (TypedExp((BinaryExp(BooleanOp("=="),exp,BoolLiteral(false))),BoolT)) false false) in
            let label_1 = (fresh_label ()) in
            let label_2 = fresh_label() in
            let stmt3_1 = (IfStmt3 (expr3, (label_1))) in
            let stmt3_label_1 = (Label3 label_1) in
            let (var3lst_1, stmt3lst_1) = (mstmts_to_IR3stmts cname curr_method stmtlist1) in
            let stmt3_2 = (GoTo3 (label_2)) in
            let (var3lst_2, stmt3lst_2) = (mstmts_to_IR3stmts cname curr_method stmtlist2) in
            let stmt3_label_2 = (Label3 label_2) in
            (exprvars@var3lst_1@var3lst_2, exprstmts@[stmt3_1]@stmt3lst_1@[stmt3_2]@[stmt3_label_1]@stmt3lst_2@[stmt3_label_2])
            
        | WhileStmt (exp, stmtlist) -> ([],[]) (*TODO*)
        | ReadStmt (var_id) -> 
            let (expr3, exprvars, exprstmts) = (mOOLexpr_to_IR3 cname (TypedExp((Var var_id),IntT)) true true) in
            let stmt3 = (ReadStmt3 (iR3Expr_get_id3 expr3)) in (exprvars, exprstmts@[stmt3])
        | PrintStmt (exp) ->  
            let (expr3, exprvars, exprstmts) = (mOOLexpr_to_IR3 cname exp true false) in
            let stmt3 = (PrintStmt3 (iR3Expr_get_idc3 expr3)) in (exprvars, exprstmts@[stmt3])
        | AssignFieldStmt (exp1, exp2) -> ([],[])
        | MdCallStmt (exp) -> ([],[]) (* TODO *)
        | ReturnStmt (exp) -> 
            let (expr3, exprvars, exprstmts) = (mOOLexpr_to_IR3 cname exp true true) in
            let retIR3 = (ReturnStmt3 (iR3Expr_get_id3 expr3)) in (exprvars, exprstmts @ [retIR3])
        | ReturnVoidStmt -> ([], [ReturnVoidStmt3])
        | AssignStmt(id, exp) -> 
          let (expr3, exprvars, exprstmts) = mOOLexpr_to_IR3 cname exp false false in 
            begin
              let assignIR3 =
                match id with
                | TypedVarId (id1,t,1) ->
                    AssignFieldStmt3 (FieldAccess3 ("this", id1), expr3)
                | TypedVarId (id1,_,2) | SimpleVarId id1 ->
                    (AssignStmt3 (id1, expr3))
                | _ -> failwith "Error in assignIR3"
              in (exprvars, exprstmts@[assignIR3])
            end
        | _ -> failwith("couldnt match statement ");

        in let (vars, stmts) = helper head in
          let (tailvars, tailstmts) = (mstmts_to_IR3stmts cname curr_method tail) in
            (vars @ tailvars, stmts @ tailstmts)
    | [] -> ([], [])

(* Transform a method to IR3 *) 
let mOOL_mddecl_to_IR3 cname m : md_decl3 = 
  let (nvars, nstmts) = mstmts_to_IR3stmts cname m m.stmts in
  { 
    id3 = string_of_var_id m.ir3id;
	  rettype3 = m.rettype ;
	  params3 = (ObjectT cname, "this") :: mOOL_var_decl_list_to_IR3 m.params ;
	  localvars3 = mOOL_var_decl_list_to_IR3 m.localvars @ nvars ; (*Add new variables here *)
    ir3stmts = nstmts;
	}
;;
(* not used anymore
let rec convert_method_list_to_ir3_second mthd_list cname list_to_fill : md_decl3 list =
  match mthd_list with
  head :: tail -> let new_list = list_to_fill@[mOOL_mddecl_to_IR3 cname head] in convert_method_list_to_ir3_second tail cname new_list
  | [] -> list_to_fill*)

let get_meth_signature m = 
  let params = m.params in List.map fst params

let meth_signature_to_unique_name mth_name type_list = 
  string_of_var_id mth_name ^ "~" ^ (string_of_list type_list string_of_mOOL_type "~" )

let mOOL_class_decl_to_IR3 (class_decl :class_decl) : class3 = 
  let (cname, ext, attr_list, md_list) = class_decl in 
  let own_var_table = mOOL_var_decl_list_to_IR3 ((List.map (fun (m,v)->v) attr_list)) in
  (* let ir3_mthds = convert_method_list_to_ir3_second md_list cname [] in *)
  let own_meth_table = List.map (fun m -> (meth_signature_to_unique_name m.mOOLid (get_meth_signature m), string_of_var_id m.ir3id))
  md_list in
  {
    classname = cname;
    parent = ext;
    var_table = own_var_table;
    meth_table = own_meth_table;
  };;

let rec convert_method_list_to_ir3 class_decl_list list_to_fill : md_decl3 list = 
  match class_decl_list with
  (cname, ext, attr_list, md_list) :: tail -> 
    let rec helper mth_list return_list = 
      (match mth_list with
      head :: tail2 -> let new_list = return_list@[mOOL_mddecl_to_IR3 cname head] in helper tail2 new_list
      | [] -> convert_method_list_to_ir3 tail return_list)
    in helper md_list list_to_fill
  | [] -> list_to_fill

(* Transform a MOOL program to IR3 *) 
let mOOL_program_to_IR3 (p:mOOL_program):ir3_program=  
  let mOOL_class_main_to_IR3 ((cname,mmthd):class_main ) : class3 * md_decl3 =
    ( {
      classname=cname ;
      parent=None ;
      var_table=[] ;
      meth_table=[] ;
      },
      (mOOL_mddecl_to_IR3 cname mmthd)) in
  begin
    let (mainclass, classes) = p in 
      let newmainir3, newmainmdir3 = mOOL_class_main_to_IR3 mainclass in (*Create new main-class and main methods*)
        let new_class_list_ir3 = List.map mOOL_class_decl_to_IR3 classes in (*Convert the class list to ir3*)
          let new_md_decl_list_ir3 = convert_method_list_to_ir3 classes [] in (*Create new methods in IR3*)
            (newmainir3::new_class_list_ir3,newmainmdir3, new_md_decl_list_ir3) (*Append new mainclass to te class list, main method in middle, rest of the methods at the end*)
  end
