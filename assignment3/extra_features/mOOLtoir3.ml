
(* ===================================================== *)
(* ============== CS4212 Compiler Design ============== *)
(*   Transformation to intermediary representation IR3   *)
(* ===================================================== *)

open MOOL_structs
open Ir3mOOL_structs

(* Return the unique label number *)
let labelcount = ref 0 
let fresh_label () = 
	(labelcount:=!labelcount+1; !labelcount)

(* Return the temporary integer identifier *)
let varcount = ref 0 
let fresh_var () = 
	(varcount:=!varcount+1; (string_of_int !varcount))

(* Return method label *)
let mthdcount = ref 0
let fresh_mthd_count () =
  (mthdcount:=!mthdcount+1; !mthdcount)

(* Init the mthd_count to zero *)
let restart_mthd_count () = (mthdcount:=0)

(* To generate a new temporary *)
let new_temporary_var () : id3 =
  "_t" ^ fresh_var()

(* Transform mOOL var_id into id3 *)
let mOOL_varid_to_id3 (vid:var_id) : id3 =
  match vid with
  | SimpleVarId id -> id
  | TypedVarId (id,t,s) -> id

(* Transform mOOL var_id to iR3Expr *)
let mOOL_varid_to_IR3Expr
  (classid:class_name)
  (v:var_id) : (ir3_exp * var_decl3 list * ir3_stmt list * ir3_type) =
  match v with
  | SimpleVarId id -> (Idc3Expr (Var3 id),[],[],Unknown)
  | TypedVarId (id,t,s) ->
    if (s==1)
    then (FieldAccess3 ("this",id),[],[],Unknown)
    else (Idc3Expr (Var3 id),[],[],Unknown)

(* Transform var_decl list into var_decl3 list *)
let mOOL_var_decl_list_to_id3 (var_decl_list:var_decl list) =
  let helper (mool_vt,mool_vid:var_decl) =
    match mool_vid with
    | SimpleVarId id -> (mool_vt,id)
    | TypedVarId (id,t,s) -> (t,id) in
  (List.map helper var_decl_list)

(* Get the string signature of method m. *)
let get_meth_signature
  (mthd:md_decl) : meth_signature =
  if (List.length mthd.params>0)
  then let mthd_signature = List.map
                              (fun (vt,vid) -> vt)
                            mthd.params in
        ((string_of_var_id mthd.mOOLid), mthd_signature)
  else (string_of_var_id mthd.mOOLid, [])

(* Map signature to an unique name: mthdname~Int~Bool ... *)
let meth_signature_to_unique_name
  (mthdname,params_type_list:string * ir3_type list) =
  let str_of_types = List.map (fun (t) -> string_of_ir3_type t) params_type_list in
  String.concat "~" (mthdname :: str_of_types)

(* Create the method global name *)
let method_global_name (cname:class_name) : string =
  let md_global_name = "_" ^ cname ^ "_" ^ string_of_int (fresh_mthd_count()-1) in
  md_global_name

(* Return the class cname among classes *)
let get_class3_in_classes3_list
  (cname:cname3)
  (classes3:class3 list) : class3 =
  List.find (fun (c3) -> String.compare cname c3.classname==0) classes3

(* Get the global name of a method from its name *)
let get_global_name_mthd
  (mthd:md_decl) (cname:class_name) (classes3:class3 list) : id3 =
  let cl = get_class3_in_classes3_list cname classes3 in
  let mthd_sig = meth_signature_to_unique_name (get_meth_signature mthd) in
  let (msig,global_name) =
    List.find (fun (meth_table_sig,gb) -> String.compare meth_table_sig mthd_sig==0)
    cl.meth_table in
  global_name

(* Get the var_table of the class cname *)
let get_class3_var_table
  (cname:cname3)
  (classes3:class3 list) : (var_decl3 list) =
  let c3 = get_class3_in_classes3_list cname classes3 in
  c3.var_table

(* Get the meth_table of the class cname *)
let get_class3_meth_table
  (cname:cname3)
  (classes3:class3 list) : (id3 * id3) list =
  let c3 = get_class3_in_classes3_list cname classes3 in
  c3.meth_table

(* NOT USED. *)
(* Apply shadowing policy between class attributes *)
let filter_own_var_table
  (own_var_table:var_decl3 list)
  (old_var_table:var_decl3 list) : (var_decl3 list) =
  List.filter
    (fun (t,id) -> (not (List.exists (fun (nt,nid) -> nid=id) old_var_table)))
  own_var_table

(* Join meth_table. Used when class A inherit from class B *)
let join_meth_tables
  (own_meth_table:(id3 * id3) list)
  (old_meth_table:(id3 * id3) list) : (id3 * id3) list =
  let rec helper (old:(id3 * id3) list) (temp_meth:(id3 * id3) list) =
    match old with
    | [] -> temp_meth
    | (nsig,nid) :: t_lst ->
      try
        (* Overrides *)
        let own_overriding = List.find (fun (signature,id) -> nsig=signature)
                             own_meth_table in
        let new_temp_meth = temp_meth @ [own_overriding] in
        helper t_lst new_temp_meth
      with Not_found ->
        let new_temp_meth = temp_meth @ [(nsig,nid)] in
        helper t_lst new_temp_meth
  in
  (* Handle overriding *)
  let meth_table = helper old_meth_table [] in
  (meth_table @ (List.filter
                  (fun (nsig,nid) -> (not (List.exists 
                                            (fun (npsig,npid) -> npid=nid)
                                          meth_table)))
                own_meth_table))

(* Transform class_decl into class3 *)
let mOOL_class_decl_to_class3
  (classes3:class3 list)
  (cname,cparent,cvars,cmthds:class_decl) : class3 =
  let own_var_table = mOOL_var_decl_list_to_id3 ((List.map (fun (m,v) -> v)
                                                  cvars)) in
  let own_meth_table =
    List.map (fun m -> (meth_signature_to_unique_name (get_meth_signature m),
                        method_global_name cname))
    cmthds in
  restart_mthd_count();
  match cparent with
  | None -> let c3 = {  classname = cname;
                        parent = None;
                        var_table = own_var_table;
                        meth_table = own_meth_table;} in c3
  | Some p ->
    let old_var_table = get_class3_var_table p classes3 in
    let old_meth_table = get_class3_meth_table p classes3 in
    (*let new_own_var_table = filter_own_var_table own_var_table old_var_table in*)
    let new_own_var_table = old_var_table @ own_var_table in
    let new_meth_table = join_meth_tables own_meth_table old_meth_table in
    { classname = cname;
      parent = Some p;
      var_table = new_own_var_table;
      meth_table = new_meth_table }
 
(* Return id3 from expression *)
let iR3Expr_to_id3
  (ir3exp:ir3_exp)
  (type_exp:ir3_type) : (ir3_stmt list * var_decl3 list * id3) =
  match ir3exp with
  | Idc3Expr (Var3 v) -> [], [], v
  | _ ->
    let temp_var = new_temporary_var() in
    ([AssignStmt3 (temp_var, ir3exp)], [(type_exp,temp_var)], temp_var)

(* Return idc3 from expression *)
let iR3Expr_to_idc3
  (ir3exp:ir3_exp)
  (type_exp:ir3_type) : (ir3_stmt list * var_decl3 list * idc3) =
  let (nstmts,nvars,ir3_id3) = (iR3Expr_to_id3 ir3exp type_exp) in
  (nstmts,nvars,Var3 ir3_id3)

(* Transform an expression into its opposite *)
let negate_expr (st:ir3_stmt) : ir3_exp =
  match st with
  | AssignStmt3 (id,exp) -> 
    begin
      match exp with
      | BinaryExp3 (op,idc1,idc2) -> BinaryExp3 (RelationalOp "==",(Var3 id),BoolLiteral3 false)
      | UnaryExp3 (op,idc) -> BinaryExp3 (RelationalOp "==",(Var3 id),BoolLiteral3 false)
      | Idc3Expr (BoolLiteral3 true) -> BinaryExp3 (RelationalOp "==",(Var3 id),BoolLiteral3 false)
      | Idc3Expr (BoolLiteral3 false) -> BinaryExp3 (RelationalOp "==",(Var3 id),BoolLiteral3 true)
      | _ -> failwith("No other expressions can be matched except BinaryExp3 and Idc3Expr.")
    end
  | _ -> failwith("Statement " ^ string_of_ir3_stmt st ^ " should be AssignStmt3.")

(* Transform mOOL expression into IR3 expression *)
let rec mOOL_exp_to_IR3Expr
  (classid:class_name)
  (mexp:mOOL_exp) : (ir3_exp * var_decl3 list * ir3_stmt list * ir3_type) =
  match mexp with
  | TypedExp (texp,t) -> 
    begin
      match texp with
      | UnaryExp (op,e) ->
        let (iR3Exp,iR3vars,iR3stmts,iR3type) = (mOOL_exp_to_IR3Expr classid e) in
        let (nstmts,nvars,iR3Exp_idc3) = (iR3Expr_to_idc3 iR3Exp t) in
        (UnaryExp3 (op, iR3Exp_idc3), iR3vars@nvars, iR3stmts@nstmts, t)
      | BinaryExp (op,e1,e2) ->
        let (iR3Exp1,iR3vars1,iR3stmts1,iR3type1) = (mOOL_exp_to_IR3Expr classid e1) in
        let (iR3Exp2,iR3vars2,iR3stmts2,iR3type2) = (mOOL_exp_to_IR3Expr classid e2) in
        let (nstmts1,nvars1,iR3Exp1_idc3) = (iR3Expr_to_idc3 iR3Exp1 iR3type1) in
        let (nstmts2,nvars2,iR3Exp2_idc3) = (iR3Expr_to_idc3 iR3Exp2 iR3type2) in
        let newExpr = BinaryExp3 (op,iR3Exp1_idc3,iR3Exp2_idc3) in
        let newVars = iR3vars1 @ iR3vars2 @ nvars1 @ nvars2 in
        let newStmts = iR3stmts1 @ iR3stmts2 @ nstmts1 @ nstmts2 in
        (newExpr,newVars,newStmts,t)
      | FieldAccess (e,vid) ->
        let (iR3Exp,iR3vars,iR3stmts,iR3type) = (mOOL_exp_to_IR3Expr classid e) in
        let (nstmts,nvars,iR3Exp_id3) = (iR3Expr_to_id3 iR3Exp iR3type) in
        let (iR3Exp_vid,iR3vars_vid,iR3stmts_vid,iR3type_vid) =
          (mOOL_varid_to_IR3Expr classid vid) in
        let (nstmts_vid,nvars_vid,iR3Exp_id3_vid) =
          (iR3Expr_to_id3 iR3Exp_vid iR3type_vid) in
        let newExpr = FieldAccess3 (iR3Exp_id3, iR3Exp_id3_vid) in
        let newVars = iR3vars @ iR3vars_vid @ nvars @ nvars_vid in
        let newStmts = iR3stmts @ iR3stmts_vid @ nstmts @ nstmts_vid in
        (newExpr,newVars,newStmts,iR3type_vid)
      | ObjectCreate cid -> (ObjectCreate3 cid,[],[],ObjectT cid)
      | MdCall (e,e_lst) ->
        (* e can be either FieldAccess (ThisWord,Var id,CastExp) or Var id  *)
        let rec helper (exprlst:mOOL_exp list) =
          match exprlst with
          | [] -> []
          | exp :: exp_tail ->
            let (iR3Exp,iR3vars,iR3stmts,iR3type) = (mOOL_exp_to_IR3Expr classid exp) in
            let (nstmts,nvars,iR3Exp_idc3) = (iR3Expr_to_idc3 iR3Exp iR3type) in
            let newVars = iR3vars @ nvars in
            let newStmts = iR3stmts @ nstmts in
            ((iR3type,iR3Exp_idc3),(newVars,newStmts)) :: (helper exp_tail)
        in
        let args = (helper e_lst) in
        begin
          let (mdId,iR3Exp_id3,iR3vars,iR3stmts,iR3type) = match e with
          | TypedExp (et,tt) -> 
            begin
            match et with
            | FieldAccess (exp,vid) ->
              let (expr3,exprvars,exprstmts,exptype) = (mOOL_exp_to_IR3Expr classid exp) in
              let (nstmts,nvars,iR3Exp_id3) = (iR3Expr_to_id3 expr3 exptype) in
              let newVars = exprvars @ nvars in
              let newStmts = exprstmts @ nstmts in
              (vid,iR3Exp_id3,newVars,newStmts,exptype)
            | Var v -> (v,"this",[],[],t)
            | _ -> failwith("error [MdCall]: expression " ^ string_of_mOOL_expr e
                           ^ " not recognised.")              
            end
          | _ -> failwith("error [MdCall]: expression " ^ string_of_mOOL_expr e
                         ^ " is not in the format TypedExp mOOL_exp * mOOL_type.")
        in
        let (args_typed_idc3_lst,args_vars_stmts) = List.split args in
        let (args_types,args_idc3_lst) = List.split args_typed_idc3_lst in
        let (args_vars,args_stmts) = List.split args_vars_stmts in
        let vars = iR3vars @ (List.flatten args_vars) in
        let stmts = iR3stmts @ (List.flatten args_stmts) in
        let temp_lst = (string_of_var_id mdId) :: (List.map string_of_ir3_type args_types) in
        let md_name = String.concat "~" temp_lst in
        (MdCall3(iR3Exp_id3,md_name,(Var3 iR3Exp_id3)::args_idc3_lst),vars,stmts,iR3type)
        end
      | BoolLiteral v -> (Idc3Expr (BoolLiteral3 v),[],[],t)
      | IntLiteral v -> (Idc3Expr (IntLiteral3 v),[],[],t)
      | StringLiteral v -> (Idc3Expr (StringLiteral3 v),[],[],t)
      | ThisWord -> (Idc3Expr (Var3 "this"),[],[],t)
      | NullWord -> (Idc3Expr (Var3 "null"),[],[],t)
      | SuperWord -> (FieldAccess3 ("this","super"),[],[],t)
      | Var v ->
        let iR3type = t in
        let (iR3Exp,iR3vars,iR3stmts,_) = (mOOL_varid_to_IR3Expr classid v) in
        let (nstmts,nvars,iR3Exp_idc3) = (iR3Expr_to_idc3 iR3Exp iR3type) in
        let newExpr = Idc3Expr iR3Exp_idc3 in
        (newExpr,iR3vars@nvars,iR3stmts@nstmts,iR3type)
      | CastExp (e,ObjectT t) ->
        let (iR3Exp,iR3vars,iR3stmts,iR3type) = (mOOL_exp_to_IR3Expr classid e) in
        let (nstmts,nvars,iR3Exp_id3) = (iR3Expr_to_id3 iR3Exp (ObjectT t)) in
        let newExp = CastExp3 (iR3Exp_id3,t) in
        let newVars = iR3vars @ nvars in
        let newStmts = iR3stmts @ nstmts in
        (newExp,newVars,newStmts,ObjectT t)
      | _ -> failwith("Unrecognised expression " ^ string_of_mOOL_expr texp)
    end
  | ThisWord -> (Idc3Expr (Var3 "this"),[],[],Unknown)
  | _ -> failwith("Cannot convert TypedExp into iR3 intermediate code in"
                  ^ " class " ^ classid ^ ".")

(* Transform mOOL stmts into IR3 stmts *)
let rec mOOL_stmts_to_IR3stmts
  (classid:class_name)
  (mthd:md_decl)
  (stmtlst:mOOL_stmt list) : (var_decl3 list * ir3_stmt list) =
  match stmtlst with
  | [] -> ([],[])
  | s :: lst -> 
    let rec helper s : (var_decl3 list * ir3_stmt list) =
      match s with
      | IfStmt (ifexp,thenstmts,elsestmts) ->
        let (expr3,exprvars,exprstmts,exptype) = (mOOL_exp_to_IR3Expr classid ifexp) in
        let (iR3elsevars,iR3elsestmts) = (mOOL_stmts_to_IR3stmts classid mthd elsestmts) in
        let (iR3thenvars,iR3thenstmts) = (mOOL_stmts_to_IR3stmts classid mthd thenstmts) in
        let codeConditionIfFalse = fresh_label() in
        let nextCodeLabel = fresh_label() in
        let (nstmts,nvars,expr3_idc3) = (iR3Expr_to_idc3 expr3 exptype) in
        let negatediR3Exp = negate_expr (List.hd (List.rev nstmts)) in
        let iR3IfStmt = IfStmt3 (negatediR3Exp,codeConditionIfFalse) in
        let newVars = exprvars@nvars@iR3thenvars@iR3elsevars in
        let newStmts = exprstmts@nstmts@[iR3IfStmt]@iR3thenstmts@[GoTo3 nextCodeLabel]@[Label3 codeConditionIfFalse]@iR3elsestmts@[Label3 nextCodeLabel] in
        (newVars,newStmts)
      | WhileStmt (e,mstmts) ->
        let (expr3,exprvars,exprstmts,exptype) = (mOOL_exp_to_IR3Expr classid e) in
        let (iR3thenvars,iR3thenstmts) = (mOOL_stmts_to_IR3stmts classid mthd mstmts) in
        let codeConditionWhile = fresh_label() in
        let nextCodeLabel = fresh_label() in
        let (nstmts,nvars,expr3_idc3) = (iR3Expr_to_idc3 expr3 exptype) in
        let negatediR3Exp = negate_expr (List.hd (List.rev nstmts)) in
        let iR3IfStmt = IfStmt3 (negatediR3Exp,nextCodeLabel) in
        let newVars = exprvars@iR3thenvars@nvars in
        let newStmts = [Label3 codeConditionWhile]@exprstmts@nstmts@[iR3IfStmt]@iR3thenstmts@[GoTo3 codeConditionWhile]@[Label3 nextCodeLabel] in
        (newVars,newStmts)
      | ReadStmt id -> ([],[ReadStmt3 (mOOL_varid_to_id3 id)])
      | PrintStmt e ->
        let (expr3,exprvars,exprstmts,exptype) = (mOOL_exp_to_IR3Expr classid e) in
        let (nstmts,nvars,printIR3_idc3) = (iR3Expr_to_idc3 expr3 exptype) in
        let newVars = exprvars @ nvars in
        let newStmts = exprstmts @ nstmts in
        (newVars,newStmts@[PrintStmt3 printIR3_idc3])
      | AssignStmt (id,e) ->
        let (expr3,exprvars,exprstmts,exptype) = (mOOL_exp_to_IR3Expr classid e) in
        begin
          let assignIR3 = match id with
          | TypedVarId (id1,t,1) -> 
            AssignFieldStmt3 (FieldAccess3 ("this", id1), expr3)
          | TypedVarId (id2,_,2) | SimpleVarId id2 ->
            (AssignStmt3 (id2, expr3))
          | _ -> failwith("error [AssignStmt]: ill-formatted id " ^ string_of_var_id id
                         ^ " in expression " ^ string_of_mOOL_expr e ^ ".")
          in (exprvars,exprstmts@[assignIR3])
        end
      | AssignFieldStmt (e1,e2) ->
        let (expr3_1,exprvars_1,exprstmts_1,exptype_1) = (mOOL_exp_to_IR3Expr classid e1) in
        let (expr3_2,exprvars_2,exprstmts_2,exptype_2) = (mOOL_exp_to_IR3Expr classid e2) in
        let newVars = exprvars_1 @ exprvars_2 in
        let newStmts = exprstmts_1 @ exprstmts_2 in
        (newVars,newStmts)
      | MdCallStmt e ->
        let (expr3,exprvars,exprstmts,exptype) = (mOOL_exp_to_IR3Expr classid e) in
        (exprvars,exprstmts@[MdCallStmt3 expr3])
      | ReturnStmt e ->
        let (expr3,exprvars,exprstmts,exptype) = (mOOL_exp_to_IR3Expr classid e) in
        let (nstmts,nvars,retIR3_id3) = (iR3Expr_to_id3 expr3 exptype) in
        let newVars = exprvars @ nvars in
        let newStmts = exprstmts @ nstmts @ [ReturnStmt3 retIR3_id3] in
        (newVars,newStmts)
      | ReturnVoidStmt -> ([],[ReturnVoidStmt3])
    in
    let (vars,stmts) = (helper s) in
    let (tailvars,tailstmts) = (mOOL_stmts_to_IR3stmts classid mthd lst) in
    (vars@tailvars, stmts@tailstmts)

(* Transform a method to IR3 *) 
let mOOL_mddecl_to_IR3
  (cname,parent,attrs,mds:class_decl) (classes3:class3 list) (mthd:md_decl) =
  let (nvars,nstmts) = mOOL_stmts_to_IR3stmts cname mthd mthd.stmts in
  let global_name = get_global_name_mthd mthd cname classes3 in
    { id3 = global_name;
      rettype3 = mthd.rettype;
      params3 = (ObjectT cname, "this") ::
                (mOOL_var_decl_list_to_id3 mthd.params);
      localvars3 = (mOOL_var_decl_list_to_id3 mthd.localvars) @ nvars;
      ir3stmts = nstmts;
    }

(* Transform mOOL md_decl/class_decl list to IR3 md3/class3 list *)
let mOOL_mthd_class_decl_list_to_IR3
  (classes:class_decl list) : (class3 list * md_decl3 list) =
  let rec helper
    (cls:class_decl list) (temp_classes3:class3 list) (temp_md3:md_decl3 list) =
    match cls with
    | [] -> (temp_classes3,temp_md3)
    | mOOL_class_decl :: mOOL_classes_tail ->
      let (cname,parent,attrs,mds) = mOOL_class_decl in
      let new_temp_classes3 =
        temp_classes3 @ [mOOL_class_decl_to_class3 temp_classes3 mOOL_class_decl] in
      let new_temp_md3 =
        temp_md3 @ (List.map (mOOL_mddecl_to_IR3 mOOL_class_decl new_temp_classes3) mds) in
      helper mOOL_classes_tail new_temp_classes3 new_temp_md3
  in
  (helper classes [] [])
 
(* Transform a MOOL program to IR3 *) 
let mOOL_program_to_IR3 (p:mOOL_program):ir3_program =  
  let mOOL_class_main_to_IR3 ((cname,mmthd):class_main) =
    let cl3_main = {  classname=cname;
                      parent=None;
                      var_table=[];
                      meth_table=[("main","main")]; } in
    let md3_main = (mOOL_mddecl_to_IR3 (cname,None,[],[]) [cl3_main] mmthd) in
    (cl3_main,md3_main) in
  begin
    let (mainclass, classes) = p in
    let (newmainir3, newmainmdir3) = (mOOL_class_main_to_IR3 mainclass) in
    let (newclasses, newmethodsir3) = (mOOL_mthd_class_decl_list_to_IR3 classes) in
    (newmainir3::newclasses, newmainmdir3, newmethodsir3)
  end
