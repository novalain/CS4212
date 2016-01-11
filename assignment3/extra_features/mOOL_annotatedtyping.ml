(* ==================================================== *)
(* ============== CS4212 Compiler Design ============== *)
(*           Static Check of mOOL programs            *)
(* ==================================================== *)

(* opening MOOL_structs give access to all its structures *)
open MOOL_structs

(* Compare two types. *)
let compare_mOOL_types (t1:mOOL_type) (t2:mOOL_type) =
  match t1,t2 with
  (* Comparing null type with a class type is always true. *)
  | (ObjectT name1), (ObjectT "null") -> true
  | (ObjectT "null"), (ObjectT name2) -> true
  | (ObjectT name1), (ObjectT name2) ->
     ((String.compare name1 name2) == 0)
  | t1, t2 -> t1 == t2

(* Compare two variable ids. *)
let compare_var_ids (v1:var_id) (v2:var_id) =
  match v1,v2 with
  | SimpleVarId id1, SimpleVarId id2 ->
     ((String.compare id1 id2) == 0)
  | SimpleVarId id1, TypedVarId (id2,t,s) ->
     ((String.compare id1 id2) == 0)
  | TypedVarId (id1,t,s), SimpleVarId id2 ->
     ((String.compare id1 id2) == 0)
  | TypedVarId (id1,t1,s1), TypedVarId (id2,t2,s2) ->
     ((String.compare id1 id2) == 0) && (s1 == s2)

(* Check that class name is contained in a class_decl list. *)
let is_cname_in_class_decl_list
  (cname:class_name) (classes:class_decl list) : bool =
  if  (List.exists (fun (name,parent,attrs,mthds) -> String.compare name cname==0)
      classes)
  then true
  else false

(* Check that a class_decl list has not dusplicates *)
let rec check_uniqueness_cnames
  (classes:class_decl list) =
  match classes with
  | [] -> true
  | (cname, _, _, _) :: tail -> 
    if (is_cname_in_class_decl_list cname tail)
    then failwith("[static checker error] : class name " ^ cname ^ " is duplicated.")
    else check_uniqueness_cnames tail

(* Get the string of the class_name option *)
let string_of_class_parent
  (cname_inh:class_name option) : string =
    match cname_inh with
    | Some pa -> pa
    | None -> ""

(* Check that the class hierarchy is acyclic *)
let check_acyclic_hierarchy
  (classes:class_decl list) : bool =
  let rec helper (classes:class_decl list) =
    match classes with
    | [] -> true
    | (cname,parent,_,_) :: tail ->
        let str_of_parent = string_of_class_parent parent in
        match str_of_parent with
        | "" -> helper tail
        | _ ->
          if (String.compare cname str_of_parent==0)
          then failwith("[static checker error]: cannot extended " ^ cname ^ " to itself.")
          (* Need to check extended class is in tail. *)
          else
          if (not (is_cname_in_class_decl_list str_of_parent tail))
          then failwith("[static checker error]: extending " ^ cname ^ " with "
                       ^ str_of_parent ^ " which might be out of scope or undefined.")
          else helper tail
  in
  (* Check hierarchy in a bottom-up way. *)
  helper (List.rev classes)

(* Transform a attr_decl list into a var_decl list for a class *)
let get_var_decl_list_from_attr_decl (class_attrs:attr_decl list) : var_decl list =
  (List.map (fun (attr_modifier, attr_var_decl) -> attr_var_decl) class_attrs)

(* Check if var_decl is contained in var_decl list *)
let rec is_var_decl_in_var_decl_list (vd_list:var_decl list) (vd:var_decl) =
  let vd_type_to_check, vd_id_to_check = vd in
  match vd_list with
  | [] -> false, (Unknown,vd_id_to_check)
  | (vd_type, vd_id) :: tail ->
      if (compare_var_ids vd_id vd_id_to_check)
      then (true,(vd_type,vd_id))
      else is_var_decl_in_var_decl_list tail vd

(* Check if var_decl list contains duplicates. Return id duplicated attribute *)
let rec check_var_decl_list_has_duplicates (vd_list:var_decl list) =
  match vd_list with
  | [] -> false, ""
  | (vd_type, vd_id) :: tail ->
    let is_vd_id_defined_in_tail, (_,_) = is_var_decl_in_var_decl_list tail (vd_type,vd_id) in
    if (is_vd_id_defined_in_tail)
    then (true,string_of_var_id vd_id)
    else check_var_decl_list_has_duplicates tail

(* Wrapper to check if class attributes are unique *)
let rec check_distinct_attrs_in_classes (classes:class_decl list) =
  match classes with
  | [] -> true
  | (cname, _,class_attrs,_) :: tail ->
    let class_attr_var_decl_list = get_var_decl_list_from_attr_decl class_attrs in
    let is_duplicated, errmsg = check_var_decl_list_has_duplicates class_attr_var_decl_list in
    (* If the var_decl of class cname has duplicates *)
    if (is_duplicated==true)
    then failwith("[static checker error]: attribute " ^ errmsg ^ " in " ^ cname ^ " is duplicated.")
    else check_distinct_attrs_in_classes tail

(* Check that same-scope are distinct *)
let rec helper_class_md_params md_params_localvars_list (cname:class_name) =
  match md_params_localvars_list with
  | [] -> true
  | (md_var_id,md_params,md_localvars) :: tail_md ->
    (* Check that parameters in the class method have unique names *)
    let params_has_duplicates, errmsg_param = (check_var_decl_list_has_duplicates md_params) in
    let locals_has_duplicates, errmsg_local = (check_var_decl_list_has_duplicates md_localvars) in
    match (params_has_duplicates,locals_has_duplicates) with
    | true,_ -> failwith ("[static checker error]: parameter " ^ errmsg_param ^ " in method "
                          ^ string_of_var_id md_var_id ^ " (class " ^ cname
                          ^ ") is duplicated.")
    | _,true -> failwith ("[static checker error]: local variable " ^ errmsg_local ^ " in method "
                          ^ string_of_var_id md_var_id ^ " (class " ^ cname
                          ^ ") is duplicated.")
    | false,false -> (helper_class_md_params tail_md cname)

(* Check defined scopes *)
let check_scopes (main_class:class_main) (classes:class_decl list) =
  (* Check that no two attributes declared within a class can have the same name *)
  let _ = (check_distinct_attrs_in_classes classes) in
  (* Check that no two parameters declared within a method have different names *)
  let (cname_main,md_main) = main_class in
  (* Create list of pair such as (cname, md_decl list) *)
  let classes_md_list =
    (cname_main,[md_main]) ::
    (List.map (fun (cname,_,_,md_list) -> (cname, md_list)) classes) in
  let rec helper_classes_md_params (classes_md_list:(class_name * md_decl list) list) =
    match classes_md_list with
    | [] -> true
    | (cname,class_md_list) :: tail_class ->
      (* Set the params and localvars from a class methods into a new list *)
      let class_md_params_list =
        (List.map (fun (md) -> (md.mOOLid,md.params,md.localvars)) class_md_list) in
      (* Execute function help_class_md_params *)
      let _ = helper_class_md_params class_md_params_list cname in
      helper_classes_md_params tail_class
  in
  (helper_classes_md_params classes_md_list)

(* Check if md1 and md2 have the same signature *)
let are_md_sig_equal (md1_list:var_decl list) (md2_list:var_decl list) : bool =
  if ((List.length md1_list)==(List.length md2_list))
  then (List.for_all2 (fun (md1_type,md1_id) (md2_type,md2_id) ->
                          compare_mOOL_types md1_type md2_type)
       md1_list md2_list)
  else false (* If signaure list have different size, then they are not equal *)

(* Search class_decl pname into a classes *)
let rec get_class_decl
  (cname:class_name)
  (pname:class_name)
  (classes:class_decl list) =
  try
  (List.find
    (fun (c,p,attrs,mds) -> String.compare pname c==0)
  classes)
  with Not_found -> failwith ("[static checker error]: cannot get inherited class "
                             ^ pname ^ " of class " ^ cname ^ ".")

(* Check for overloading and overriding in inherited classes *)
let rec check_md_over_in_inheritance 
  (md:md_decl)
  (parent_md_list:md_decl list)
  (cname:class_name)
  (pname:class_name) =
  (* Check overloading in inheritance *)
  match parent_md_list with
  | [] -> false
  | md_hd :: pmd_tail -> 
    if ((compare_var_ids md.mOOLid md_hd.mOOLid) &&
       (are_md_sig_equal md.params md_hd.params))
    then if (compare_mOOL_types md.rettype md_hd.rettype)
         then false (* Method md overrides method md_hd in its parent's class pname. *)
            (* Method fail to override. *)
         else failwith("[static checker error]: method " ^ string_of_var_id md.mOOLid
                      ^ " in class " ^ cname ^ " is trying to overload/override method "
                      ^ string_of_var_id md_hd.mOOLid ^ " in parent class " ^ pname ^ "."
                      ^ " Check method's type return and/or signature.")
    else check_md_over_in_inheritance md pmd_tail cname pname

(* Check method overloading and overriding *)
let rec check_overloading_overriding (classes:class_decl list) =
  (* Check if a method is overloaded in a list of methods *)
  let rec helper_overloading_mds_in_class (md_list:md_decl list) (cname:class_name) =
    match md_list with
    | [] -> false
    | last_md :: [] -> false
    | md :: tail_md ->
      if (List.for_all (fun md_in_tail ->
                          (compare_var_ids md.mOOLid md_in_tail.mOOLid) &&
                          (are_md_sig_equal md.params md_in_tail.params))
         tail_md)
      then failwith("[static checker error]: method " ^ string_of_var_id md.mOOLid
                   ^ " in class " ^ cname ^ " is duplicated (same name and signature).")
      else helper_overloading_mds_in_class tail_md cname (* Methods overloading is verified *)
  in (* end helper_overloading_mds_in_class *)
  let rec helper_overloading_overriding_in_inheritance
    (cname:class_name)
    (parent_name:class_name)
    (md_list_cname:md_decl list)
    (classes:class_decl list) =
    match parent_name with
    | "" -> false (* Class cname doesn't have inherit. No need to check the tree *)
    | _ ->
      let (pname,ppname,pattrs,pmds) = (get_class_decl cname parent_name classes) in
      (* Loop over the methods in cname. *)
      let rec helper_check_mds_cname_in_inh (md_list_cname:md_decl list) =
        match md_list_cname with
        | [] -> false
        | md_hd :: md_tail -> 
          (* Check for overloading/overriding in parent's class *)
          let _ = (check_md_over_in_inheritance md_hd pmds cname pname) in
          (helper_check_mds_cname_in_inh md_tail)
      in (* end helper_check_mds_cname_in_inh *)
      (* If cname has a parent, check whether there is no incorrect overloading/overriding *)
      let _ = (helper_check_mds_cname_in_inh md_list_cname) in
      (* Keep checking for parent of parent classes till there is no more *)
      (helper_overloading_overriding_in_inheritance cname (string_of_class_parent ppname)
                                                    md_list_cname classes)
  in (* end helper_overloading_overriding_in_inheritance *)
  match classes with
  | [] -> false
  | (cname,parent,_,md_list) :: tail_classes ->
    (* Check if some methods which overload incorrectly within the class cname *)
    let _ = (helper_overloading_mds_in_class md_list cname) in (* No overloading on class mds *)
    (* Check within the inherits classses if there is overloading/overriding *)
    let _ = (helper_overloading_overriding_in_inheritance cname (string_of_class_parent parent)
                                                                md_list classes) in
    (* Keep checking for next classes. *)
    check_overloading_overriding tail_classes

(* Build the scope. Transform the SimpleVarId into TypedVarId *)
let build_scope (vars:var_decl list) (scope:int) =
  let helper ((var_type,varid):var_decl) =
    match varid with
    | SimpleVarId id -> (var_type, TypedVarId (id,var_type,scope))
    | TypedVarId (id,t,s) -> (var_type, TypedVarId (id,var_type,scope))
  in
  (List.map helper vars) (* Apply the helper_scope to each var_decl in vars *)

(* Return the type of var_decl vid within the environment *)
let get_type_vid_in_env
  (env:var_decl list) (vid:var_id) (md_id:var_id) (cname:class_name) =
  try
    (List.find (fun (v_type_scope,v_id_scope) -> compare_var_ids v_id_scope vid) env)
  with Not_found -> failwith("[type check error|Scope]: variable "
                            ^ string_of_var_id vid ^ " in class " ^ cname
                            ^ ", method " ^ string_of_var_id md_id
                            ^ " is not defined. Make sure"
                            ^ " the variable can be accessed within the current"
                            ^ " scope.")

(* Build the list of classes that cname inherited from. *)
let get_inherited_classes_chain
  (cname,parent,attrs,vars:class_decl)
  (classes:class_decl list) : (class_decl list) =
  let rec helper (chain:class_decl list) (p:class_name option) =
    match p with
    | None -> (* If parent's class is None, cannot define objects of other classes *)
      if (List.length chain==0)
      then failwith("[type check error|InheritedChain]: class " ^ cname
                   ^ " cannot cast anything since it inherit"
                   ^ " from any class. Check class extension.")
      (* List chain has at least one parent and this parent hasn't a parent.
         Here the chain is well constructed so the progarm returns it *)
      else chain
    (* Parent classes has been checked during the static checking. *)
    (* Class cname has some inherited class. We need to add it to the chain. *)
    | Some _ -> 
      (* Get the parent information of class cname. *)
      let (pname,pp,pattrs,plocalvars) =
        (get_class_decl cname (string_of_class_parent p) classes) in
      let new_chain = (pname,pp,pattrs,plocalvars) :: chain in
      helper new_chain pp
  in
  helper [] parent (* Launch the chain construction. *)

(* Check if signatures of methods md1 and md2 are equal *)
let are_md_sig_equal_simplified
  (md1_params:mOOL_type list) (md2_params:mOOL_type list) : bool =
  if ((List.length md1_params)==(List.length md2_params))
  then (List.for_all2
          (fun md1_type md2_type -> compare_mOOL_types md1_type md2_type)
       md1_params md2_params)
  else false

(* Return the method md_id in class cname mdths *)
let get_md_in_class
  (classes:class_decl list)
  (cname:class_name)
  (md_id:var_id)
  (md_params:mOOL_type list) : (md_decl) =
  (* Return the class information. *)
  let (_,_,_,mds) = (List.find (fun (n,p,a,m) -> String.compare n cname==0) classes) in
  try (* Find the method in class methods mds. *)
    (List.find (
      fun (md) ->
        let md_type_params = (List.map (fun (t,id) -> t) md.params)
          in
        (compare_var_ids md.mOOLid md_id) &&
        (are_md_sig_equal_simplified md_params md_type_params))
    mds)
  with Not_found -> failwith("[type check error|SearchMethod]: method "
                            ^ string_of_var_id md_id ^ " in class " ^ cname
                            ^ " has not been" ^ " defined.")

(* Type check the class attributes *)
let type_check_attrs_class
  (main,classes:mOOL_program)
  (vars:var_decl list)
  (c,parent:class_name * class_name option) : bool =
  let rec helper (v:var_decl list) =
    match v with
    | [] -> false
    | (v_type,v_id) :: v_tail ->
      begin
        match v_type with
        | ObjectT cname ->
          let inh_chain = (get_inherited_classes_chain (c,parent,[],[]) classes) in
          if (is_cname_in_class_decl_list cname inh_chain==true)
          then (helper v_tail)
          else
            if (String.compare cname c==0)
            then failwith("[type check error|AttrDeclCheck]: attribute " ^ string_of_var_id v_id
                          ^ " in class " ^ c ^ " cannot have the same type of the class.")
            else failwith("type check error|AttrDeclCheck]: attribute " ^ string_of_var_id v_id
                          ^ " in class " ^ c ^ " has an undefined type " ^ cname ^ ".")
        | _ -> (helper v_tail)
      end
    in
  (helper vars)

(* Get the inherited environment of class cname *)
let get_inherited_environment
  (p:mOOL_program)
  (cname,parent,attrs,mds:class_decl)
  (classes:class_decl list) =
  let rec helper (inh_env:var_decl list) (p:class_name) =
    match p with
    | "" -> inh_env (* No more parents. *)
    | _ ->
      (* Get the information of the cname's parent. *)
      let (p_cname,p_parent,p_attrs,p_mds) = (get_class_decl cname p classes) in
      let vars_of_attrs = (get_var_decl_list_from_attr_decl p_attrs) in
      let rec helper_shadowing (super_env:var_decl list) (updated_env:var_decl list) =
        match super_env with
        | [] -> updated_env
        | (super_vt,super_vid) :: tail ->
          (* Check whether a variable defined in a parent class is shadowed in
             a subclass *)
          if (List.exists (fun (t,id) -> compare_var_ids super_vid id) updated_env)
          then (helper_shadowing tail updated_env)
          else
          (* Append variable to existing environment *)
          let new_env = (super_vt, super_vid) :: updated_env in
          (helper_shadowing tail new_env)
      in (* end helper_shadowing *)
      let new_inh_env = (helper_shadowing vars_of_attrs inh_env) in
      (helper new_inh_env (string_of_class_parent p_parent))
  in (* end helper *)
  (* Init the inh_env with attributes of cname. Can be empty *)
  let _ = (type_check_attrs_class p (get_var_decl_list_from_attr_decl attrs) (cname,parent)) in
  let env = (helper (get_var_decl_list_from_attr_decl attrs) (string_of_class_parent parent)) in
  if (List.length env>=1) then (build_scope env 1)
  else env

(* Type check an expression and returns (TypedExp of mOOL_exp * mOOL_type) *)
let type_check_expr
  (main_class,classes:mOOL_program)
  (cname,parent,attr,mds:class_decl)
  (md_scope:var_decl list)
  (md_id:var_id)
  (expr:mOOL_exp) : (mOOL_exp * mOOL_type) =
  let rec helper_check_expr (mOOLexpr:mOOL_exp) =
    match mOOLexpr with
    | UnaryExp (op,e) ->
      let (e_exp,e_type) = (helper_check_expr e) in
      begin (* Check whether operator is applied to the good types *)
        match (op,e_type) with
        (* Check the !false and !true expressions.
           The program need to check the value of op since it can be applied
           to a BoolT and a IntT. Indeed, ! cannot be applied to an IntT *)
        | (UnaryOp op, BoolT) ->
          if (String.compare op "!"==0)
          then (TypedExp (UnaryExp (UnaryOp op,e_exp), BoolT), BoolT)
          else failwith("[type check error|UnaryExp]: unary operator " ^ op
                       ^ " in class " ^ cname ^ ", method "
                       ^ string_of_var_id md_id ^ ", expression "
                       ^ string_of_mOOL_expr expr ^ " cannot be applied to type "
                       ^ string_of_mOOL_type e_type ^ ".")
        (* Check the -x with x an interger expressions *)
        | (UnaryOp op, IntT) ->
          if (String.compare op "-"==0)
          then (TypedExp (UnaryExp (UnaryOp op,e_exp), IntT), IntT)
          else failwith("[type check error|UnaryExp]: unary operator " ^ op
                       ^ " in class " ^ cname ^ ", method "
                       ^ string_of_var_id md_id ^ ", expression "
                       ^ string_of_mOOL_expr expr ^ " cannot be applied to type "
                       ^ string_of_mOOL_type e_type ^ ".")
        | (_,_) ->
          failwith("[type check error|UnaryExp]: unary operator " ^ string_of_mOOL_op op
                  ^ " in class " ^ cname ^ ", method " ^ string_of_var_id md_id
                  ^ ", expression " ^ string_of_mOOL_expr expr
                  ^ " and type " ^ string_of_mOOL_type e_type ^ " are incompatibles.")
      end
    | BinaryExp (op,e1,e2) ->
      let (e1_exp,e1_type) = (helper_check_expr e1) in
      let (e2_exp,e2_type) = (helper_check_expr e2) in
      begin
        match (op,e1_type,e2_type) with
        (* Check expressions like true AND false, true OR false.
           The BooleanOp op is always only applied to (BoolT,BoolT), then no
           need to check whether it is equal to AND or OR *)
        | (BooleanOp op, BoolT, BoolT) ->
          (TypedExp (BinaryExp (BooleanOp op, e1_exp, e2_exp), BoolT), BoolT)
        | (AritmeticOp op, IntT, IntT) ->
          (TypedExp (BinaryExp (AritmeticOp op, e1_exp, e2_exp), IntT), IntT)
        (* Types t1 and t2 need to be equal and the operator should be defined *)
        | (RelationalOp op, IntT, IntT) ->
          begin
            match op with
            | "==" | ">" | ">=" | "<" | "<=" | "!=" ->
              (TypedExp (BinaryExp (RelationalOp op, e1_exp, e2_exp), BoolT), BoolT)
            | _ -> failwith("[type check error|BinaryExp]: relational operator "
                           ^ op ^ " cannot be recognised in "
                           ^ "expression " ^ string_of_mOOL_expr expr ^ " since "
                           ^ "it is applied to non Int expressions.")
          end
        | (_,_,_) -> failwith("[type check error|BinaryExp]: cannot find a binary expression"
                             ^ " between " ^ string_of_mOOL_expr e1 ^ " and "
                             ^ string_of_mOOL_expr e2 ^ " with operator "
                             ^ string_of_mOOL_op op ^ " in class " ^ cname
                             ^ ", method " ^ string_of_var_id md_id)
      end
    | FieldAccess (e,vid) ->
      let (e_exp,e_type) = (helper_check_expr e) in
      (* Program need to check that the id is an attribute of class e_type *)
      let (_,_,attrs,_) = (get_class_decl cname (string_of_mOOL_type e_type) classes) in
      (* Build the var_decl list from the attr_decl list. *)
      let vdecl_class = (get_var_decl_list_from_attr_decl attrs) in
      let (is_var_decl_defined,(vid_type,vid)) =
        (is_var_decl_in_var_decl_list vdecl_class (Unknown, vid)) in
      (* Check if the class e_type contains the attribute vid. *)
      if (not is_var_decl_defined) (* Var vid is not defined *)
      then failwith("[type check error|FieldAccess]: expression " ^ string_of_mOOL_expr e
                   ^ " in class " ^ cname ^ " incorrect. Either the object or"
                   ^ " the method " ^ string_of_var_id vid ^ " are not defined.")
      else
      (* Need to check the private modifier of att_decl variables *)
      let (v_mod,_) =
        (List.find (fun (v_mod_lst,(vt_lst,vid_lst)) -> compare_var_ids vid_lst vid)
        attrs) in
      begin
        match v_mod with
        | Private ->
          if (compare_mOOL_types (ObjectT cname) e_type)
          then (TypedExp (FieldAccess (e_exp, vid), vid_type), vid_type)
          else failwith("[type check error|FieldAccess]: trying to access the private field "
                       ^ string_of_var_id vid ^ " of expression "
                       ^ string_of_mOOL_expr e ^ " from class " ^ cname ^ ".")
        | Public -> (TypedExp (FieldAccess (e_exp, vid), vid_type), vid_type)
      end
    | ObjectCreate c ->
      if (is_cname_in_class_decl_list c classes)
      then (TypedExp (ObjectCreate c, ObjectT c), ObjectT c)
      else failwith("[type check error|ObjectCreate] [ObjectCreate]: cannot create "
                   ^ "an object of class " ^ c ^ " because class " ^ c ^ " is undefined.")
    | MdCall (e,e_list) -> (* Call can be either local or global. *)
      let helper_e_list_expr lst =
        let (lst_checked,_) = (helper_check_expr lst) in lst_checked in
      let helper_e_list_type lst =
        let (_,lst_typed) = (helper_check_expr lst) in lst_typed in
      let params = (List.map helper_e_list_type e_list) in (* Get signature. *)
      let e_list_checked = (List.map helper_e_list_expr e_list) in
      let (m_expr,m_type) =
        begin
          match e with
          (* Prgram ensures that methods with modifier private can only be
             accessed by objects belonging to the same class *)
          | FieldAccess (expr,md_name_called) ->
            (* Check the expression from which the method is called *)
            let (e_checked,e_typed) = (helper_check_expr expr) in
            begin
              match expr with
              | ThisWord ->
                (* Program find the method with name md_id within the class. *)
                let md_called = (get_md_in_class classes cname md_name_called params) in
                (TypedExp ((FieldAccess (ThisWord, md_called.mOOLid)), md_called.rettype), md_called.rettype)
              | Var id ->
                begin
                  match e_typed with
                  | ObjectT c ->
                    (* Error if there is no method called md_name_called in class c *)
                    let md_called = (get_md_in_class classes c md_name_called params) in
                    (* Ex: e.m1(). Here e and m1 belong to the same class. then
                       there is no need to check the m1's modifier. *)
                    if (String.compare cname c==0)
                    then
                    (TypedExp ((FieldAccess (e_checked, md_called.mOOLid)), md_called.rettype), md_called.rettype)
                    else
                    (* Only methods not overrided with private modifier cannot
                       be accessed from outside their class
                       Program checks if md_called is overrided in current class. *)
                      if (List.exists (
                           fun (md) -> ((compare_var_ids md.mOOLid md_called.mOOLid) &&
                                        (are_md_sig_equal md.params md_called.params) &&
                                        (compare_mOOL_types md.rettype md_called.rettype)))
                         mds)
                      then
                        (* True: use the method in current class. No need to take
                           care of the method's modifier since it's overrided *)
                      let md_overriding = (get_md_in_class classes cname md_name_called params) in
                        (* LocalCall. *)
                      (TypedExp ((FieldAccess (e_checked, md_overriding.mOOLid)), md_overriding.rettype), md_overriding.rettype)
                      (* False, means there is no method in current class that
                         overrides the method called. *)
                      else
                        if (md_called.modifier==Private)
                        then failwith("[type check error|MdCall]: "
                                     ^ "trying to access the private method "
                                     ^ string_of_var_id md_called.mOOLid
                                     ^ " of class " ^ c ^ " in class " ^ cname
                                     ^ ", in method " ^ string_of_var_id md_id
                                     ^ " and in expression " ^ string_of_mOOL_expr e
                                     ^ " from object " ^ string_of_var_id id
                                     ^ " of type " ^ c ^ ".")
                        else
                        (* Object id and method md_called belong to same class. *)
                        (TypedExp ((FieldAccess (e_checked, md_called.mOOLid)), md_called.rettype), md_called.rettype)
                  | _ -> failwith("[type check error|MdCall]: impossible to call a"
                                 ^ " method on typed object " ^ string_of_mOOL_type e_typed
                                 ^ " in expression " ^ string_of_mOOL_expr expr
                                 ^ " in method " ^ string_of_var_id md_id ^ ", class "
                                 ^ cname ^ ".")
                end
              (* Calling a method on a on-the-fly casted object. cast_type
                 is already defined/verified. Example: ( (B)t ).m1() *)
              | CastExp (e, ObjectT cast_type) ->
                let md_called = (get_md_in_class classes cast_type md_name_called params) in
                (* No need to check the modifier since we are in the class. *)
                if (String.compare cast_type cname==0)
                then
                (TypedExp ((FieldAccess (e_checked, md_called.mOOLid)), md_called.rettype), md_called.rettype)
                else
                if (not (md_called.modifier==Private))
                then
                (TypedExp ((FieldAccess (e_checked, md_called.mOOLid)), md_called.rettype), md_called.rettype)
                else failwith("[type check error|MdCall]: trying"
                                ^ " to call the private method "
                                ^ string_of_var_id md_id ^ " of class " ^ cast_type
                                ^ " from class " ^ cname ^ " in expression "
                                ^ string_of_mOOL_expr expr ^ ".")
              | _ -> failwith("[type check error|MdCall]: FieldAccess not "
                             ^ "known by the compiler in method " ^ string_of_var_id md_id
                             ^ ", class " ^ cname ^ ". Hasta la vista, baby!")
            end
          (* Local call. id is the name of method called. *)
          | Var md_called_id ->
            let md_called = (get_md_in_class classes cname md_called_id params) in
            (TypedExp ((FieldAccess (ThisWord, md_called.mOOLid)), md_called.rettype), md_called.rettype)
          | _ -> failwith("[type check error|MdCall]: expression "
                         ^ string_of_mOOL_expr e ^ " undefined in method "
                         ^ string_of_var_id md_id ^ ", class " ^ cname ^ ".")
        end
      in
      (TypedExp (MdCall (m_expr, e_list_checked), m_type), m_type)
    | BoolLiteral v -> (TypedExp (BoolLiteral v, BoolT), BoolT)
    | IntLiteral v -> (TypedExp (IntLiteral v, IntT), IntT)
    | StringLiteral v -> (TypedExp (StringLiteral v, StringT), StringT)
    | ThisWord -> (TypedExp (ThisWord, ObjectT cname), ObjectT cname)
    (* NullWord is related to any class. Then the program uses "null" keyword *)
    | NullWord -> (TypedExp (NullWord, ObjectT "null"), ObjectT "null")
    | SuperWord ->
      let str_parent = (string_of_class_parent parent) in
      (TypedExp (SuperWord, ObjectT str_parent), ObjectT str_parent)
    (* var_id id should be contained in current method's env noted md_env *)
    | Var id ->
      (* Is the variable in the env? If it is found, it returns the var_decl
         of var_id corresponding in the environment *)
      let (v_type,v_id) = get_type_vid_in_env md_scope id md_id cname in
      (TypedExp (Var v_id, v_type), v_type)
    (* Casting only happens with the inherited classes. How to handle it:
       1. Build the ordered list of inherited classes, e.g. if C extends B
          and B extends A, then the inherited classes of C are
          [class_decl B, class_decl A].
       2. Type check the expression casted.
       3. Output warning if casting type and expression type are identical. 
       4. Check whether the casting type is in the inherited hierarchy of cname. *)
    | CastExp (e,cast_type) ->
      (* Check first if the class is allowed to cast (is there inheritance?).
         Error are thrown inside get_inherited_classes_chain if parent is null.*)
      let casting_class = (get_class_decl cname (string_of_mOOL_type cast_type) classes) in
      let inh_chain = (get_inherited_classes_chain casting_class classes) in
      (* There is at least one class_decl in inherited_chain. Type check expr *)
      let (e_expr,e_type) = (helper_check_expr e) in
      (* Output warning if casting type is identical to expression type. *)
      if (compare_mOOL_types e_type cast_type)
      then 
      (print_endline("[warning|CastExp]: casting expresssion of type "
                     ^ string_of_mOOL_type e_type ^ " into "
                     ^ string_of_mOOL_type cast_type ^ ". Useless.");
      (TypedExp (CastExp (e_expr,cast_type), cast_type), cast_type))
      (* Otherwise, need to check that the casting type is in the inheritance *)
      else
      let chain_cname =
        if (String.compare cname "Main"==0)
        then ([string_of_mOOL_type cast_type] @ List.map (fun (n,p,a,m) -> n) inh_chain)
        else ([string_of_mOOL_type cast_type;cname] @ (List.map (fun (n,p,a,m) -> n) inh_chain))
      in
      (* If casting type is in the inherited classes, then isOK *)
      if  (List.exists (
            fun (n) -> compare_mOOL_types (ObjectT n) cast_type)
          chain_cname)
      then (TypedExp (CastExp (e_expr,cast_type), cast_type), cast_type)
      else failwith("[type check error|CastExp]: trying to cast to an undefined class "
                    ^ string_of_mOOL_type cast_type ^ " the expression \""
                    ^ string_of_mOOL_expr e ^ "\"" ^ " in class " ^ cname
                    ^ ", method " ^ string_of_var_id md_id ^ ".")
    | _ -> failwith ("[type check error|FatalError]: expression " ^ string_of_mOOL_expr mOOLexpr
                    ^ " in class " ^ cname ^ ", method " ^ string_of_var_id md_id
                    ^ " couldn't be matched with any expression defined in the grammar.")
  in
  (helper_check_expr expr)

(* Type check the statements. Return the statement return type and its checked
   statements list. From a list of statemetns, the steps followed are:
   1. For each statement, type check its expressions and return its type.
   2. The program outputs a new list of statements with a return type and the
      typed expressions within the statements. *)
let rec type_check_stmts
  (p:mOOL_program) (cname,parent,attrs,vars:class_decl) (md:md_decl)
  (stmts:mOOL_stmt list) (md_scope:var_decl list) : (mOOL_type * mOOL_stmt list) =
  let (main_class,classes) = p in
  match stmts with
  | [] -> (md.rettype, []) (* Method without declared statements. *)
  | stmt :: tail_stmts ->
    let rec helper_type_check_stmts (s:mOOL_stmt) =
      match s with
      | IfStmt (if_expr,then_stmts,else_stmts) ->
        let (if_expr_expr,if_expr_type) =
          (type_check_expr p (cname,parent,attrs,vars) md_scope md.mOOLid if_expr) in
        if (not (compare_mOOL_types if_expr_type BoolT)) (* if_expr should have type BoolT. *)
          then failwith("[type check error|IfStmt]: expression "
                       ^ string_of_mOOL_expr if_expr_expr ^ " in class " ^ cname
                       ^ ", method " ^ string_of_var_id md.mOOLid
                       ^ " has type " ^ string_of_mOOL_type if_expr_type
                       ^ " instead of type BoolT.")
        else
        let (then_stmts_rettype,then_stmts_checked) =
          (type_check_stmts p (cname,parent,attrs,vars) md then_stmts md_scope) in
        let (else_stmts_rettype, else_stmts_checked) =
          (type_check_stmts p (cname,parent,attrs,vars) md else_stmts md_scope) in
        begin
          (* Check type block: t = t1 U t2 as stated in the rules *)
          match (then_stmts_rettype,else_stmts_rettype) with
          (* When comparing two types, one type can an ObjectT and the other "null"
             (see method compare_mOOL_types). We need to catch the non "null" type. *)
          | (ObjectT t1), (ObjectT "null") ->
            (ObjectT t1, IfStmt (if_expr_expr,then_stmts_checked,else_stmts_checked))
          | (ObjectT "null"), (ObjectT t2) -> 
            (ObjectT t2, IfStmt (if_expr_expr,then_stmts_checked,else_stmts_checked))
          | (ObjectT t1), (ObjectT t2) ->
            if (compare_mOOL_types (ObjectT t1) (ObjectT t2))
            then (ObjectT t1, IfStmt (if_expr_expr,then_stmts_checked,else_stmts_checked))
            else failwith("[type check error|IfStmt]: return type in"
                         ^ " if else then statement " ^ string_of_mOOL_stmt stmt
                         ^ " imcompatible between else and then statements.")
          | t1, t2 ->
            if (compare_mOOL_types t1 t2)
            then (t1, IfStmt (if_expr_expr,then_stmts_checked,else_stmts_checked))
            else failwith("[type check error|IfStmt]: return type"
                         ^ " between else and then statement " ^ string_of_mOOL_stmt stmt
                         ^ "in method " ^ string_of_var_id md.mOOLid
                         ^ ", class " ^ cname ^ " are incompatibles.")
        end
      | WhileStmt (while_cond_expr,while_stmts_list) ->
        let (while_cond_checked,while_cond_type) =
          (type_check_expr p (cname,parent,attrs,vars) md_scope md.mOOLid while_cond_expr) in
        if (not (compare_mOOL_types while_cond_type BoolT))
        then failwith("[type check error|WhileStmt]: expression "
                     ^ string_of_mOOL_expr while_cond_expr ^ " in class " ^ cname
                     ^ ", method " ^ string_of_var_id md.mOOLid
                     ^ " has type " ^ string_of_mOOL_type while_cond_type
                     ^ " instead of type BoolT.")
        else
        let (while_stmts_rettype,while_stmts_checked) =
          (type_check_stmts p (cname,parent,attrs,vars) md while_stmts_list md_scope) in
        (while_stmts_rettype, WhileStmt (while_cond_checked,while_stmts_checked))
      | ReadStmt id -> (* Judgement is dom(gamma)(v)=t where t e {int, bool, string} *)
        let (vtype,vid) = (get_type_vid_in_env md_scope id md.mOOLid cname) in
        begin
          match vtype with
          | (ObjectT c) ->
            failwith("[type check error|ReadStmt]: variable " ^ string_of_var_id id
                    ^ " should have type Int, Bool or String. Type " ^ c ^ " not accepted.")
          | Unknown ->
            failwith("[type check error|ReadStmt]: variable " ^ string_of_var_id id
                    ^ " should have type Int, Bool or String. Type Unknown "
                    ^ "unrecognised.")
          | _ -> (VoidT, ReadStmt id)
        end
      | PrintStmt e ->
        let (e_checked,e_type) =
          (type_check_expr p (cname,parent,attrs,vars) md_scope md.mOOLid e) in
        begin
          match e_type with
          | (ObjectT c) ->
            failwith("[type check error|PrintStmt]: to be print, expression "
                    ^ string_of_mOOL_expr e ^ " should have type "
                    ^ " Int, Bool or String instead of " ^ c ^ ".")
          | Unknown ->
            failwith("[type check error|PrintStmt]: to be print, expression "
                    ^ string_of_mOOL_expr e ^ " should have type "
                    ^ " Int, Bool or String instead of Unknown type.")
          | _ -> (VoidT, PrintStmt e_checked)
        end
      (* Implicit casting example: A t = new B(); is handled *)
      | AssignStmt (vid, e) ->
        let (e_checked,e_type) =
          (type_check_expr p (cname,parent,attrs,vars) md_scope md.mOOLid e) in
        let (vtype,vid_typed) = (get_type_vid_in_env md_scope vid md.mOOLid cname) in
        begin
          match e_type, vtype with
          | (Unknown,_) | (_,Unknown) ->
            failwith("[type check error|AssignStmt]: the type of one member is "
                    ^ "Unknown in class " ^ cname ^ ", method "
                    ^ string_of_var_id md.mOOLid ^ ", expression \""
                    ^ string_of_mOOL_expr e ^ "\".")
          | (t1,t2) ->
            if (compare_mOOL_types t1 t2)
            then (VoidT, AssignStmt (vid_typed,e_checked))
            else
            let inherited_chain_class_decl =
              if (String.compare cname "Main"==0)
              then classes
              else (get_inherited_classes_chain (cname,parent,attrs,vars) classes) in
            let inherited_cnames = (List.map (fun (n,p,a,m) -> n) inherited_chain_class_decl) in
            (* class A. class B extends A. A a; a = new B(); Upcasting check. *)
            if (List.exists
                  (fun (inh_type) -> compare_mOOL_types vtype (ObjectT inh_type))
               inherited_cnames)
            then (VoidT, AssignStmt (vid_typed,e_checked))
            else failwith("[type check error|AssignStmt]: types in statement "
                         ^ string_of_mOOL_stmt s ^ " are imcompatibles. Type "
                         ^ string_of_mOOL_type vtype ^ " of var "
                         ^ string_of_var_id vid ^ " in class " ^ cname ^ ", method "
                         ^ string_of_var_id md.mOOLid ^ " mismatch type " ^ cname ^ ".")
        end
      (* From mOOL_parser.mly: { AssignFieldStmt ( FieldAccess ( $1, $3), $5 ) }.
         e1 is the FieldAccess ($1,$3) and e2 the $5. *)
      | AssignFieldStmt (e1, e2) ->
        (* If in FieldAccess, $3 is not defined (doesn't belong to the attributes
           of the e1's type, then raise an error *)
        let (e1_checked,e1_type) =
          (type_check_expr p (cname,parent,attrs,vars) md_scope md.mOOLid e1) in
        let (e2_checked,e2_type) =
          (type_check_expr p (cname,parent,attrs,vars) md_scope md.mOOLid e2) in
        (* e1_type and e2_type should have same type so that expr is correct. *)
        if (compare_mOOL_types e1_type e2_type)
        then (VoidT, AssignFieldStmt (e1_checked,e2_checked))
        else
        failwith("[type check error|AssignFieldStmt]: incompatibles types in method "
                ^ string_of_var_id md.mOOLid ^ ", class " ^ cname ^ " in "
                ^ "statement " ^ string_of_mOOL_stmt s)
      (* From mOOL_parser.mly: { MdCallStmt (MdCall ( $1, $3 )) }.
         Expression e can be either SLocalCall or SGlobalCall. This is handle
         within the type_check_expr of e. Here, we just need to check the type. *)
      | MdCallStmt e ->
        let (e_checked,e_type) =
          (type_check_expr p (cname,parent,attrs,vars) md_scope md.mOOLid e) in
        begin
          match e_type with
          | Unknown ->
            failwith("[type check error|MdCallStmt]: type of expression "
                    ^ string_of_mOOL_expr e ^ " in method "
                    ^ string_of_var_id md.mOOLid ^ ", class " ^ cname
                    ^ " is Unknown. Make sure it is well defined.")
          | _ -> (e_type, (MdCallStmt e_checked))
        end
      | ReturnStmt e ->
        let (e_checked,e_type) =
          (type_check_expr p (cname,parent,attrs,vars) md_scope md.mOOLid e) in
        begin
          match e_type with
          | Unknown ->
            failwith("[type check error|ReturnStmt]: return type of expression "
                    ^ string_of_mOOL_expr e ^ " is Unknown in class "
                    ^ cname ^ ", method " ^ string_of_var_id md.mOOLid ^ ".")
          | _ ->
            if (compare_mOOL_types e_type md.rettype)
            then (md.rettype, (ReturnStmt e_checked))
            else failwith("[type check error|ReturnStmt]: return type of expression "
                         ^ string_of_mOOL_expr e ^ " (type " ^ string_of_mOOL_type e_type
                         ^ ") with method " ^ string_of_var_id md.mOOLid
                         ^ " (type " ^ string_of_mOOL_type md.rettype ^ ") in class "
                         ^ cname ^ " are imcompatibles.")
        end
      | ReturnVoidStmt ->
        if (compare_mOOL_types md.rettype VoidT)
        then (VoidT, ReturnVoidStmt)
        else failwith("[type check error|ReturnVoidStmte]: trying to use a Void "
                     ^ "return type in method " ^ string_of_var_id md.mOOLid
                     ^ ", class " ^ cname ^ " whereas method "
                     ^ string_of_var_id md.mOOLid ^ " has type return "
                     ^ string_of_mOOL_type md.rettype ^ ".")
    in (* end of helper *)
    (* Start of the statements processing. One statement is processed at a time *)
    let (stmt_rettype,stmt_checked) = (helper_type_check_stmts stmt) in
    (* When the program is checking a mOOL_stmt list, he needs to ensure that
       the ReturnStmt and ReturnVoidStmt are only used when the tail_stmts is
       empty. Otherwise the program consider it as a misuse. *)
    match (stmt_rettype,stmt_checked,tail_stmts) with
    | (_, ReturnStmt expr, st::tail_st) ->
      failwith("[type check error|CheckingStatements]: statement ReturnStmt is "
              ^ "wrongly used in class " ^ cname ^ ", method "
              ^ string_of_var_id md.mOOLid ^ ". Indeed, "
              ^ "it should be used after all statements.")
    | (_, ReturnVoidStmt, st::tail_st) ->
      failwith("[type check error|CheckingStatements]: statement ReturnStmt is "
              ^ "wrongly used in class " ^ cname ^ ", method "
              ^ string_of_var_id md.mOOLid ^ ". Indeed, "
              ^ "it should be used after all statements.")
    (* Here, algorithm can keep processing other statements. If stmt_checked is
       a return statement, obviously the tail_stmts is empty *)
    | (_,_,_) ->
      let (md_stmts_rettype,md_stmts_checked) =
        (type_check_stmts p (cname,parent,attrs,vars) md tail_stmts md_scope) in
      (md_stmts_rettype,(stmt_checked :: md_stmts_checked))

(* cl_env contains the variables with their scope already. Both params and
   localvars can shadow the fields in the class *)
let get_class_method_env
  (md:md_decl) (cl_env:var_decl list) (cname:class_name) : (var_decl list) =
  (* Prgram checks that params and localvars are different *)
  let rec helper_unicity_params_localvars
    (params:var_decl list)
    (locals:var_decl list) =
    match params with
    | [] -> true
    | (vt,vid) :: tail ->
      if (List.exists (fun (vt_local,vid_local) -> compare_var_ids vid vid_local)
          locals)
        then failwith ("[type check error|ClassMdEnv]: parameter/local variable "
                      ^ string_of_var_id vid ^ " in method " ^ string_of_var_id md.mOOLid
                      ^ " of class " ^ cname ^ " cannot be shadowed by a parameter/"
                      ^ "local variable.")
      else helper_unicity_params_localvars tail md.localvars
  in (* end helper_unicity_params_localvars *)
  let rec helper (vars:var_decl list) (updated_env:var_decl list) =
    match vars with
    | [] -> updated_env
    | (vt,vid) :: tail ->
      (* ll is empty if vid is not in updated_env. Otherwise it's a list with
         only one element corresponding to the class attribute with same name
         as vid. For the rest, we keep lr *)
      let (ll,lr) =
        (List.partition (fun (vt_env,vid_env) -> compare_var_ids vid vid_env)
        updated_env) in
      let new_env = (vt, TypedVarId (string_of_var_id vid,vt,2)) :: lr in
      (helper tail new_env)
  in (* end helper *)
  let _ = (helper_unicity_params_localvars md.params md.localvars) in
  let temp_env = (helper md.params cl_env) in
  (helper md.localvars temp_env)

(* Type check the class_main *)
let type_check_class_main (main_class,classes:mOOL_program) : class_main =
  let (cname_main,md_main) = main_class in
  (* Create list of declared class types in order to check the localvars definition *)
  let classes_names = (List.map (fun (c,p,a,m) -> c) classes) in
  let rec helper (vd_lst:var_decl list) =
    match vd_lst with
    | [] -> true
    | (vt,vid) :: tail ->
      begin
        match vt with
        | ObjectT c -> 
          if (List.exists (fun (cl_name) -> String.compare c cl_name==0) classes_names)
          then helper tail
          else failwith("[type check error|MdMain]: local variable " ^ string_of_var_id vid
                       ^ " in method main has an undefined attribute " ^ c ^ ".")
        | IntT | BoolT | StringT -> helper tail
        | _ -> failwith("[type check error|MdMain]: type " ^ string_of_mOOL_type vt
                       ^ " of variable " ^ string_of_var_id vid ^ " in method main."
                       ^ " is undefined.")
      end
  in (* end helper *)
  let env = (get_class_method_env md_main [] "main") in
  let _ = (helper env) in
  let md_main_env = (build_scope env 2) in
  (* Return the stmts return type and the typed statements *)
  let (stmts_rettype,stmts_checked) =
    (type_check_stmts (main_class,classes) (cname_main,Some "",[],[])
                      md_main md_main.stmts md_main_env) in
  (* Check compatibility between statements return type and method's return type *)
  let md_main_checked = match md_main.rettype, stmts_rettype with
  | VoidT, VoidT -> {md_main with stmts=stmts_checked}
  | _, _ ->
    failwith ("[type check error|MdMain]: return type of method main \"Void\""
             ^ " is not compatible with return type of its statements: "
             ^ string_of_mOOL_type stmts_rettype ^ ".")
  in (cname_main,md_main_checked)

(* Ensure that var_decl in vars have know types. *)
let type_check_var_decl_list
  (main_class,classes:mOOL_program)
  (vars:var_decl list)
  (md_id:var_id)
  (c,parent:class_name * class_name option) =
  let rec helper (v:var_decl list) =
    match v with
    | [] -> false
    | (v_type,v_id) :: v_tail ->
      begin
        (* type is BOOL_KWORD, INT_KWORD, STRING_KWORD, VOID_KWORD or CLASS_ID *)
        match v_type with
        (* If the variable belongs to a non primitive type, need to check that
           the class has been defined on the program *)
        | ObjectT cname ->
          let inh_chain = (get_inherited_classes_chain (c,parent,[],[]) classes) in
          if (is_cname_in_class_decl_list cname inh_chain==true || (String.compare cname c==0))
          (* Need to check the shadowing policy *)
          then (helper v_tail)
          else failwith("[type check error|VarDecl]: variable " ^ string_of_var_id v_id
                       ^ " in method " ^ string_of_var_id md_id ^ " of class "
                       ^ c ^ " has an undefined type " ^ cname ^ ". Please check that "
                       ^ "class " ^ cname ^ " has been declared.")
        | _ -> (helper v_tail) (* If var is has primitive type, then it is valid. *)
      end
  in
  (helper vars)

(* Type check a method. Return the typed checked method.
   Type checking is built following the Appendix A.
   1. Check that var_decl list contains variables with known types. 
   2. Type check the statements:
      a. Create the method's scope.
      b. Check that statements are correct
      c. Check compatible types with method's return and stmts return *)
let type_check_md_decl
  (p:mOOL_program)
  (cl_env:var_decl list) (* We asssume that env contains already typed variables *)
  (cname,parent,attrs,vars:class_decl)
  (md:md_decl) : md_decl =
  (* The local environment is augmented with the types of the parameters
     declared in the method, and the return type of the method, associated
     with an unique special identifier Ret. *)
  (* Check that all var_decl types are known. Raise error within the method *)
  let _ = (type_check_var_decl_list p (List.append md.params md.localvars)
                                    md.mOOLid
                                    (cname,parent)) in
  (* Create the scope associated with the method declaration in order to type
     check the statements then. Scope 1 is the class' scope. Scope 2 is the
     method's scope. Merge the environment of the class (attributes) with the
     params/locals of the method. env is supposed to be already in a typed format.*)
  let md_env = (get_class_method_env md cl_env cname) in
  (* Type check the stmts. Return typed statements and the return type of the
     statements *)
  let (stmts_rettype,stmts_checked) =
    (type_check_stmts p (cname,parent,attrs,vars) md md.stmts md_env) in
  (* Check compatibility between statements return type and method's rettype. *)
  match (md.rettype,stmts_rettype) with
  | (VoidT,VoidT) -> {md with stmts=stmts_checked}
  | (ObjectT type1), (ObjectT type2) -> (* User defined types comparison *)
    if (String.compare type1 type2==0)
    then {md with stmts=stmts_checked}
    else failwith("[type check error|MdDeclCheck]: return type in method"
                 ^ string_of_var_id md.mOOLid ^ ", class " ^ cname
                 ^ "isn't compatbile with return type of its statements."
                 ^ "Method " ^ string_of_var_id md.mOOLid ^ " expects a type return "
                 ^ string_of_mOOL_type md.rettype ^ " instead of "
                 ^ string_of_mOOL_type stmts_rettype ^ ".")
  | (type1, type2) -> (* Primitive types comparison. *)
    if (compare_mOOL_types type1 type2)
    then {md with stmts=stmts_checked}
    else failwith("[type check error|MdDeclCheck]: return type in method"
                 ^ string_of_var_id md.mOOLid ^ ", class " ^ cname
                 ^ "isn't compatbile with return type of its statements.")

(* Type check all other classes except main class *)
let rec type_check_classes
  (main,cls:mOOL_program)
  (cls_complete:class_decl list) : (class_decl list) =
  let helper_type_check_classes
    (cname,parent,attrs,mds:class_decl) : class_decl =
    (* Create the inherited environment of class cname. Contain also the variables
       that cname class inherit from extends inheritance *)
    let cl_env =
      (get_inherited_environment (main,cls_complete) (cname,parent,attrs,mds) cls_complete) in
    (cname,parent,attrs,
    (List.map (fun (md) ->
      type_check_md_decl (main,cls_complete) cl_env (cname,parent,attrs,mds) md)
    mds))
    in
  match cls with
  | [] -> []
  | cl :: cl_tail ->
    (helper_type_check_classes cl) :: (type_check_classes (main,cl_tail) cls_complete)

(* TypeCheck a MOOL Program *)
let type_check_mOOL_program
  (p:mOOL_program) : mOOL_program = 
  let (main_class,classes) = p in
  let (cname_mc,md_mc) = main_class in
  (* Check that main class name is not used in classes names *)
  if (is_cname_in_class_decl_list cname_mc classes)
  then failwith("[static checker error]: class name \"Main\" is duplicated.")
  else
  print_endline("[success] Static check: \"Main\" class name is unique.");
  (* Check uniqueness of class names *)
  let _ = (check_uniqueness_cnames classes) in
  print_endline("[success] Static check: class names are unique.");
  (* Check that the class hierarchy is acylic *)
  let _ = (check_acyclic_hierarchy classes) in
  print_endline("[success] Static check: class hierarchy is acylic.");
  (* Check of distinct same-scope identifiers. *)
  let _ = (check_scopes main_class classes) in
  print_endline("[success] Static check: same-scopes are unique.");
  (* Check overloading/overriding policy *)
  let _ = (check_overloading_overriding (List.rev classes)) in
  print_endline("[success] Static check: overloading/overriding is correct.");
  (* Start the type checking for class_main and class_decl list. *)
  let newmainclass = (type_check_class_main p) in
  print_endline ("[success]: Type checking main class.");
  let newclasses = (type_check_classes p classes) in
  print_endline ("[success]: Type checking classes.");
  (newmainclass,newclasses) (* Return the new typed tree. *)