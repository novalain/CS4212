
(* ==================================================== *)
(* ============== CS4212 Compiler Design ============== *)
(*           Static Check of mOOL programs            *)
(* ==================================================== *)

open MOOL_structs

(* Compare two types *)   
let compare_mOOL_types 
      (t1:mOOL_type) (t2:mOOL_type) = 
  match t1,t2 with 
  | (ObjectT name1), (ObjectT "null") -> true
  | (ObjectT name1), (ObjectT name2) -> 
     ((String.compare name1 name2) == 0) 
  | t1, t2 -> t1 == t2
          
(* Compare two variable ids *)  
let compare_var_ids v1 v2 =
  match v1,v2 with
  | SimpleVarId id1, SimpleVarId id2 -> 
     ((String.compare id1 id2) == 0)
  | SimpleVarId id1, TypedVarId (id2,t,s) -> 
     ((String.compare id1 id2) == 0)  
  | TypedVarId (id1,t,s), SimpleVarId id2 -> 
     ((String.compare id1 id2) == 0)    
  | TypedVarId (id1,t1,s1), TypedVarId (id2,t2 ,s2) ->
     ((String.compare id1 id2) == 0) && (s1 == s2)

let compare_var_decl var_decl1 var_decl2 =
  match var_decl1, var_decl2 with
  (type1, var_id1), (type2, var_id2) ->
    (compare_mOOL_types type1 type2) && 
    (compare_var_ids var_id1 var_id2)
 
let rec compare_var_decl_list lst1 lst2 =
  match lst1, lst2 with
  t1::tail1, t2::tail2 -> 
    (compare_var_decl t1 t2) &&
      (compare_var_decl_list tail1 tail2)
  | [], [] -> true
  | _, _ -> false

(* Compare two methods declarations *)  
let methods_overriding method1 method2 =
  (compare_var_ids method1.mOOLid method2.mOOLid) &&
  (compare_mOOL_types method1.rettype method2.rettype) &&
  (compare_var_decl_list method1.params method2.params)

(* Methods are the same but different return type i.e not valid*)
let invalid_method method1 method2 =
  (compare_var_ids method1.mOOLid method2.mOOLid) &&
  (compare_mOOL_types method1.rettype method2.rettype == false) &&
  (compare_var_decl_list method1.params method2.params)

(* Returns all class names as a list of strings *)
let get_class_names lst = 
  List.map (fun (a,b,c,d) -> a) lst

(* For debugging ONLY *)
let rec print_classes all_classes = 
  match all_classes with 
  (c,_,_,_) :: tail -> print_string("\n this class " ^ c); print_classes tail
  | [] -> () 

let rec print_attr all_attributes = 
  match all_attributes with
  head :: tail -> print_string("\n“this attr " ^ string_of_attr_decl head); print_attr tail
  | [] -> ()

let rec print_var_id_list var_id_list = 
  match var_id_list with
  head :: tail -> print_string("\n this var_decl " ^ string_of_var_id head); print_var_id_list tail
  | [] -> ()

let rec print_statement_list stmt_list = 
  match stmt_list with 
  head :: tail -> print_string("\n this stmt " ^ string_of_mOOL_stmt head); print_statement_list tail
  | [] -> ()
(*
let get_scope_variables lst = 
  List.map (fun (a,b) -> (match b with SimpleVarId str -> str) ) lst*)

(*
let get_all_parents lst = 
 List.map ( fun (a,b,c,d) -> (match b with None -> "" | Some str -> str)) lst*)

(* Check if a variable id exists in var_decl_list *)
let check_if_attr_id_exists attr_decl_list var_id: bool =
  let is_id_equal ((m,vd): attr_decl) : bool =
    let (mtype, vid) = vd in
      compare_var_ids vid var_id 
  in (List.exists is_id_equal attr_decl_list)  

let get_mth_list_in_class lst = 
 List.map ( fun (a) -> string_of_var_id a.mOOLid ) lst 

let rec check_for_unique_class_name
  (class_decl_list:class_decl list) =
  match class_decl_list with
  (cname,parent,_,_) :: tail -> (* Or head and (cname,_,_,_) and match head *)
      if (List.exists (fun (n) ->
            String.compare cname n == 0)
          (get_class_names tail)) (* Checks if cname exists in the list of all classes - cname*)
        then failwith ("Duplicated names, " ^ cname ^ " is a duplicate") (*there are duplicates.*)
      else check_for_unique_class_name tail
  | [] -> ()

(* For debugging a list *)
let print_list lst = 
  print_string(string_of_list(lst) (fun a -> a) ",")

(* Class can only be extended AFTER it has been declared before *)
let rec check_inheritance 
  (class_decl_list : class_decl list) (prev_decl_classes : class_name list) = 
  match class_decl_list with 
  (cname,parent,_,_) :: tail ->
      (match parent with
      Some pa -> 
        if (String.compare cname pa == 0) then failwith("Class " ^ pa ^ "extends itself")
        else if (List.exists (fun (n) -> String.compare pa n == 0) (prev_decl_classes)) then
           (let new_list = prev_decl_classes@[cname]; in check_inheritance tail new_list)
        else failwith("No class to extend..")
      | None -> let new_list = prev_decl_classes@[cname]; in check_inheritance tail new_list)
  | [] -> ()


(*
let add_to_class (a_list : string list) = 
  let new_list = "MDEO" :: a_list; in print_list(new_list)*)
(*aaaaaaa
a aaaaaa*)

(* Invalid methods is when they are the same but different return type *)
let rec check_for_invalid_methods (class_decl_list : class_decl list) = 
  match class_decl_list with 
  (cname,_,_,md_list) :: tail ->
    (* Loops all methods*)
    let rec helper list_of_md_in_class = 
      match list_of_md_in_class with 
        head :: tail2 -> 
          (* Loops the rest of the methods*)
          let rec helper2 rest_of_methods = 
            match rest_of_methods with
            head2 :: tail3 -> 
              if(invalid_method head head2) then 
                failwith ( "Methods " ^ string_of_var_id head.mOOLid ^ 
                  " and " ^ string_of_var_id head2.mOOLid ^ 
                  " in class " ^ cname ^ " can't have the same signature with " ^
                  "different return type")
              else helper2 tail3
            | [] -> helper tail2 (* proceed back *)
          in helper2 tail2
        (* Empty method list, proceeed back*)
        | [] -> check_for_invalid_methods tail
    in helper md_list
  | [] -> () 

(* Checks that methods parameter names is unique *)
let rec check_for_distinct_parameters_method (class_decl_list:class_decl list) = 
  match class_decl_list with 
    (_,_,_,md_list) :: tail ->
      let rec helper list_of_md_in_class = 
        match list_of_md_in_class with 
          head :: tail2 -> 
            let rec helper2 var_decl_list prev_decl_vars  = 
              match var_decl_list with 
              (_, var_decl) :: tail3 -> 
                (match var_decl with
                 var_id -> 
                  if( List.exists (fun (n) -> compare_var_ids var_id n ) (prev_decl_vars) ) then
                  failwith(string_of_var_id(var_id) ^ " is not a unique method parameter in method " ^ string_of_var_id head.mOOLid)
                  else let new_list = prev_decl_vars@[var_id] in helper2 tail3 new_list)
              | [] -> helper tail2 (* proceed back *)
            in helper2 head.params []
          (* Empty method list, proceeed back*)
          | [] -> check_for_distinct_parameters_method tail
      in helper md_list
    | [] -> ()

(* Throws an error if names of local variables is not unique *)
let rec check_for_distinct_local_vars_method (class_decl_list:class_decl list) = 
  match class_decl_list with 
    (_,_,_,md_list) :: tail ->
      let rec helper list_of_md_in_class = 
        match list_of_md_in_class with 
          head :: tail2 -> 
            let rec helper2 var_decl_list prev_decl_vars  = 
              match var_decl_list with 
              (_, var_decl) :: tail3 -> 
                (match var_decl with
                 var_id -> 
                  if( List.exists (fun (n) -> compare_var_ids var_id n ) (prev_decl_vars) ) then
                  failwith(string_of_var_id(var_id) ^ " is not a unique local variable in method " ^ string_of_var_id head.mOOLid)
                  else let new_list = prev_decl_vars@[var_id] in helper2 tail3 new_list)
              | [] -> helper tail2 (* proceed back *)
            in helper2 head.localvars []
          (* Empty method list, proceeed back*)
          | [] -> check_for_distinct_local_vars_method tail
      in helper md_list
    | [] -> ()

(* Throws an error if class attributes is not unique *)
let rec check_for_distinct_class_attr (class_decl_list:class_decl list) = 
  match class_decl_list with 
    (_,_,attribute_list,_) :: tail ->
      let rec helper list_of_vars_in_class prev_decl_vars = 
        match list_of_vars_in_class with 
          (_, var_decl) :: tail2 -> 
            (match var_decl with 
            (_, var_id) -> 
              if( let vid = string_of_var_id var_id in List.exists (fun (n) -> String.compare vid n == 0) (prev_decl_vars) ) then
                failwith(string_of_var_id(var_id) ^ " is not a unique name in class")
              else let new_list = prev_decl_vars@[string_of_var_id var_id] in helper tail2 new_list)
          | [] -> check_for_distinct_class_attr tail
      in helper attribute_list []
    | [] -> ()

(* Returns false if class does not exist *)
let check_if_class_exists (class_to_check : string) (class_decl_list : class_decl list): bool = 
  if (List.exists (fun (n) -> String.compare class_to_check n == 0) (get_class_names class_decl_list))
    then true
  else false

(* Make the local environment typed and store them in a list*)
let create_local_env var_decl_list scope : var_id list =
  List.map ( fun (var_type, var_id) -> 
    match var_id with
    SimpleVarId id -> TypedVarId(id, var_type, scope) 
    | TypedVarId (a,b,c) -> failwith("Creating local env error: Variable is already type-checked")) var_decl_list
(* Make the class environment typed and store it in a list*)
(* Add EVERYTHING, we already cleared the private variables away *)
let create_class_env attr_decl_list scope : var_id list =
  let return_list = [] in 
    let rec helper attr_decl_list1 the_list = 
      match attr_decl_list1 with
      (modf, (m_type, var_id)) :: tail ->
        (match modf with 
        Private -> (match var_id with (* Only gonna math simple var id here *)
          SimpleVarId id -> let new_list = the_list@[TypedVarId(id, m_type, scope) ] in helper tail new_list
          | TypedVarId (a,b,c) -> failwith("Type-check Error : Var is already type-checked"))
        | Public -> 
          (match var_id with (* Only gonna math simple var id here *)
          SimpleVarId id -> let new_list = the_list@[TypedVarId(id, m_type, scope) ] in helper tail new_list
          | TypedVarId (a,b,c) -> failwith("Type-check Error: Couldn't create class env, var is already type checked") (* not gonna happen *)
          )
        )
      | [] -> the_list 
    in helper attr_decl_list return_list

(* Throws an error if trying to declare instances of unexisting classes *)
let rec check_if_var_decl_list_is_ok var_decl_list class_decl_list =
  match var_decl_list with
  | (mool_type, var_id) :: tail ->
    (match mool_type with
    | ObjectT cname -> 
      if check_if_class_exists cname class_decl_list == false then
        failwith(cname ^ " does not exist")
      else check_if_var_decl_list_is_ok tail class_decl_list
    | _ -> check_if_var_decl_list_is_ok tail class_decl_list)
  | [] -> ()

let rec check_type_method_env_is_valid method_env class_decl_list = 
  match method_env with 
  head :: tail -> 
    (match head with 
    TypedVarId (s,t,i) ->
      (match t with
      ObjectT cname -> 
        if check_if_class_exists cname class_decl_list == false then
        failwith(cname ^ " does not exist")
      | _ -> check_type_method_env_is_valid tail class_decl_list)
    | _ -> failwith("wrong here lah"))
  | [] -> ()

let rec find_class_in_class_list cname class_decl_list : class_decl = 
  match class_decl_list with
  (curr_cname, modif, attr_list, md_list) :: tail -> 
    if( String.compare curr_cname cname == 0 ) then (curr_cname, modif, attr_list, md_list) 
    else find_class_in_class_list cname tail
  | [] -> failwith ("Type Error: could not find class: " ^ cname)

(* Finds the typed var in environment *)
let rec find_typed_var_in_env var_to_find typed_var_env : typed_var_id =
  match typed_var_env with
    head :: tail -> 
      if(compare_var_ids var_to_find head) then
        match head with
        TypedVarId (s,t,i) -> (s,t,i)
        | _ -> failwith("Compiler Error: Variable matched but not type checked..")
      else find_typed_var_in_env var_to_find tail 
    | [] -> failwith("Type-check error: Could not find variable " ^ string_of_var_id var_to_find)

(* Find the declared var type in env*)
let rec find_var_type var_to_find typed_var_env : var_decl = 
  match typed_var_env with
  (m_type, var_id) :: tail ->
    if compare_var_ids var_to_find var_id then (m_type, var_id)
    else find_var_type var_to_find tail 
  | [] -> failwith("Type-check error: Could not find variable " ^ string_of_var_id var_to_find)

let rec match_var_id_with_class_attributes var_to_match attr_decl_list cname class_decl_list : mOOL_type=
  match attr_decl_list with
  (modifier, var_decl) :: tail ->
    let (m_type, var_id) = var_decl in
      if(compare_var_ids var_id var_to_match) then 
        (match m_type with
        ObjectT name -> 
          if check_if_class_exists name class_decl_list == false then
            failwith(name ^ " does not exist")
          else (
            match modifier with
            Private -> failwith("\nType-check error: " ^ name ^ " is private, can't be accessed")
            | Public -> m_type
          )
        | _ -> m_type)
      else match_var_id_with_class_attributes var_to_match tail cname class_decl_list
  | [] -> failwith("Type-check error: No such member " ^ string_of_var_id var_to_match ^ " in class " ^ cname) 

let rec find_var_type_in_class var_id cname class_decl_list : mOOL_type = 
  (* Check if class exists*)
  let (_,_,attr_list,_) = find_class_in_class_list cname class_decl_list in 
    match_var_id_with_class_attributes var_id attr_list cname class_decl_list

(*Checks if a type is in the list of all the super classes of the current class*)
let rec is_super_class type_to_check all_super : bool = 
  let is_name_equals ((a,b,c,d): class_decl) : bool =
    let cname = string_of_mOOL_type type_to_check in 
      String.compare a cname == 0
  in (List.exists is_name_equals all_super) 

(* Compare two mOOL type lists FUN *)   
let rec compare_mOOL_type_list l1 l2 =
  match l1, l2 with
  | t1::tail1, t2::tail2 -> 
    (compare_mOOL_types t1 t2) && (compare_mOOL_type_list tail1 tail2)
  | [], [] -> true
  | _, _ -> false 

(* Compare one MOOL_type_list and one var_decl_list FUN *)   
let rec compare_mOOL_type_list_var_decl
  (l1:mOOL_type list) (l2: var_decl list) =
  let l2_type = List.map (fun (typ, vid) -> typ) l2 in
  compare_mOOL_type_list l1 l2_type

(*Finds method parameters in md declaration list FUN *)
let rec find_mth_id_params_in_list md_decl_list md_id md_params_type_list curr_class const_all_super_classes =
  match md_decl_list with
    | [] -> (*IF empty list, look for method in all the super classes*)
      (match const_all_super_classes with
        (next_cname, _,_,next_md_decl_list) :: tail_class -> 
          find_mth_id_params_in_list next_md_decl_list md_id md_params_type_list curr_class tail_class
        | [] -> failwith ("Type-check error: Matching method " ^ string_of_var_id md_id ^ " in class" ^ curr_class ^ " not found or is declared private")
      )
    | curr_method::tail_md ->
      (match curr_method.modifier with
      Private -> find_mth_id_params_in_list tail_md md_id md_params_type_list curr_class const_all_super_classes
      | Public -> 
        if (compare_var_ids curr_method.mOOLid md_id) &&
          (compare_mOOL_type_list_var_decl md_params_type_list curr_method.params)
        then curr_method
        else (find_mth_id_params_in_list tail_md md_id md_params_type_list curr_class const_all_super_classes))

(* Match current class and search for the the find the id method parameters in list*)
let find_method_id_params_in_class_decl_list class_decl_list current_cname md_id md_params_type const_all_super_classes  = 
  let tmp_class = find_class_in_class_list current_cname class_decl_list in
    let (_,_,_,md_decl_list) = tmp_class in
      find_mth_id_params_in_list md_decl_list md_id md_params_type current_cname const_all_super_classes

(* Matches an expression with a typed expression, it should already be typed by here. Think I could do it in a smarter way but ocaml skillz bad*)
let get_typed_exp exp = 
  match exp with
  TypedExp (e,t) -> (e,t) 
  | _ -> failwith "Uh oh, wrong in type checking"

(*let type_check_method_call class_decl_list env cname (exp, explist) = 
  TypedExp(exp, ObjectT "Some function - TODO")*)

let rec type_check_expression (exp : mOOL_exp) (current_cname :string) (env : var_id list) (class_decl_list : class_decl list) (const_all_super_classes : class_decl list): mOOL_exp =
  match exp with 
  UnaryExp (op, exp) -> 
    let next_exp = type_check_expression exp current_cname env class_decl_list const_all_super_classes in 
    let (_, next_type) = get_typed_exp (next_exp) in
    (match next_type, op with 
      (BoolT, UnaryOp op1) -> 
          if String.compare op1 "!" != 0 then failwith "\nType-check error in Unary Expression!" 
            else TypedExp(UnaryExp (op, next_exp), BoolT)
      | (IntT, UnaryOp op1) -> 
          if String.compare op1 "-" != 0 then failwith "\nType-check error in Unary Expression!" 
            else TypedExp(UnaryExp (op, next_exp), IntT)
      | _, _ -> failwith("\nType-check error: Unary operator fail!"))
  | BinaryExp (op, exp1, exp2) -> 
      let next_exp = type_check_expression exp1 current_cname env class_decl_list const_all_super_classes in
      let next_exp2 = type_check_expression exp2 current_cname env class_decl_list const_all_super_classes in 
      let _, next_type = get_typed_exp (type_check_expression exp1 current_cname env class_decl_list const_all_super_classes )in 
      let _, next_type2 = get_typed_exp (type_check_expression exp2 current_cname env class_decl_list const_all_super_classes )in 
      (match (op, next_type, next_type2) with
        (BooleanOp op1, BoolT, BoolT) -> TypedExp (BinaryExp (op, next_exp, next_exp2), BoolT)
      | (RelationalOp op1, IntT, IntT) -> TypedExp (BinaryExp (op, next_exp, next_exp2), BoolT)
      | (AritmeticOp op1, IntT, IntT) -> TypedExp (BinaryExp (op, next_exp, next_exp2), IntT)
      | (RelationalOp op1, type1, type2) ->
        (match op1 with 
          "==" 
        | "!=" -> 
          if (compare_mOOL_types type1 type2)
            then TypedExp (BinaryExp (op, next_exp, next_exp2), BoolT)
              else failwith ("Type-check error in Binary Expressions " ^ 
                              string_of_mOOL_type type1 ^ " and " ^ string_of_mOOL_type type2 ^ " are not comparable")
        | _ -> failwith "\nType-check error in Binary Expressions, Wrong operator")
      | (_, _, _) -> failwith("Type-check error in Binary Expressions " ^ 
                      string_of_mOOL_type next_type ^ " and " ^ 
                      string_of_mOOL_type next_type2 ^ " are not comparable"))
  | FieldAccess (exp, var_id) -> 

    (* Keep recursing "left" until we hit a variable *) (*e.g return a.b.c;*)
    let next_exp = type_check_expression exp current_cname env class_decl_list const_all_super_classes in 
    let _, next_type =  get_typed_exp ( type_check_expression exp current_cname env class_decl_list const_all_super_classes ) in
      (match next_type with
      ObjectT cname -> (*Ok, trying to access an object*)
        (* We know for sure that the left most variable exists in scope AND that the var is an object*)
        (* Check the member*)
        let new_type = find_var_type_in_class var_id cname class_decl_list in 
          TypedExp(FieldAccess(next_exp, var_id), new_type)
      | _ -> failwith(string_of_mOOL_type next_type ^ " Is not an object"))
  | ObjectCreate cname -> 
        if(check_if_class_exists cname class_decl_list) then 
          TypedExp(exp, ObjectT cname)
        else failwith("Type check error: Class " ^ cname ^ " in " ^ string_of_mOOL_expr exp ^ " does not exists")
 
  | MdCall (exp, exp_list) -> 

      (* Used for mapping the expressions to a list*)
      let get_mOOL_expr e = 
        let res = type_check_expression e current_cname env class_decl_list const_all_super_classes in
          res in

      let get_mOOL_type e = 
        let res = type_check_expression e current_cname env class_decl_list const_all_super_classes in
          let _,res = get_typed_exp(res) in res in

      let mapped_type_list = List.map get_mOOL_type exp_list in
      let mapped_exp_list = List.map get_mOOL_expr exp_list in

      (let m_type, e_expr = match exp with
      | FieldAccess (object_name_e, method_name) -> 

        let object_name_expr = type_check_expression object_name_e current_cname env class_decl_list const_all_super_classes in
        let _, object_name_type = get_typed_exp(object_name_expr) in 
        (match object_name_type with
        | ObjectT c_name -> 
          let method_decl = find_method_id_params_in_class_decl_list class_decl_list c_name method_name mapped_type_list const_all_super_classes in
          method_decl.rettype, TypedExp( FieldAccess (object_name_expr, method_name), object_name_type)
        | _ ->  failwith  ("Type-check error: Could not recognize object"))

      | Var method_name ->
        (*Find method declaration*)
        let method_decl = find_method_id_params_in_class_decl_list class_decl_list current_cname method_name mapped_type_list const_all_super_classes in
        (*Return the returned type of the method as the type of this expression*)
        method_decl.rettype, TypedExp(exp, ObjectT current_cname)
      | _ -> failwith  ("Type-check error: Trying to match something weird : " ^ string_of_mOOL_expr exp ^ "\n")
      in TypedExp (MdCall (e_expr, mapped_exp_list), m_type))
  
  | BoolLiteral m_bool -> TypedExp(BoolLiteral m_bool, BoolT)
  | IntLiteral m_int ->  TypedExp(IntLiteral m_int, IntT)
  | StringLiteral m_string -> TypedExp(StringLiteral m_string, StringT)
  | ThisWord -> TypedExp(exp, ObjectT current_cname)
  | NullWord -> TypedExp(exp, ObjectT "null")
  | SuperWord -> 
      (*Check what class is super, ONLY checking the first one at the moment*)
      (match const_all_super_classes with
      (cname,_,_,_) :: tail -> TypedExp(exp, ObjectT cname)
      | [] -> failwith "Type-check error: Superclass not found")
  | Var var_id ->
      let (s,t,i) = find_typed_var_in_env var_id env in
          TypedExp(Var(TypedVarId(s,t,i)),t)
  | CastExp (exp, m_type) ->
    let next_exp = type_check_expression exp current_cname env class_decl_list const_all_super_classes in 
    let _ , next_type = get_typed_exp(next_exp) in 
      (match m_type with
      ObjectT cname -> 
        if check_if_class_exists cname class_decl_list then 
            TypedExp(CastExp(next_exp, m_type), ObjectT cname) (* Check if class exists and give new type *)
        else failwith("Type-check error: Can't cast to a class that does not exists : " ^ cname);
      | _ -> TypedExp(CastExp(next_exp, m_type), next_type))(* Just give new type*)
  | _ -> failwith("Type-check error: Could not match expression with anything.")


(* Returns a list with type-checked statements and a return-type*)
let rec type_check_statements type_method_env class_env curr_method current_cname class_decl_list const_all_super_classes : mOOL_type * mOOL_stmt list = 
  (* temp solution ? merge class variables with method variables, we should have got rid of duplicates by here*)
  let type_method_env = List.append type_method_env class_env in

    check_type_method_env_is_valid type_method_env class_decl_list;
    (* print_var_id_list type_method_env; *)

    let return_statements = [] in 
      let rec helper ret_type statements return_list = 
        match statements with
        head :: tail ->
          begin
            match head with 
            IfStmt (exp, then_stmtlist, else_stmtlist2) -> (*Match if stmt*) (*1*) (* expression must be boolean*) (* statement lists same return type*)
              let new_exp = type_check_expression exp current_cname type_method_env class_decl_list const_all_super_classes in
              let _, new_type = get_typed_exp(new_exp) in 
                if(compare_mOOL_types new_type BoolT == false) then failwith("Type-check error in if statement: " ^ string_of_mOOL_expr new_exp)
                else
                  let new_ret_type_then, sub_then_stmts = helper Unknown then_stmtlist [] in 
                    let new_ret_type_else, sub_else_stmts = helper Unknown else_stmtlist2 [] in 
                      if(compare_mOOL_types new_ret_type_then new_ret_type_else ||
                         compare_mOOL_types new_ret_type_then VoidT ||
                         compare_mOOL_types new_ret_type_else VoidT) then
                        let new_list = return_list@[IfStmt(new_exp, sub_then_stmts, sub_else_stmts)] in helper new_ret_type_then tail new_list
                     
                     else (print_string("\ntype then: " ^ string_of_mOOL_type new_ret_type_then);
                          print_string("\ntype else: " ^ string_of_mOOL_type new_ret_type_else);
                        failwith("\nType-check error: If statement in method " ^ string_of_var_id curr_method.mOOLid ^ " does not have the same return statements"))
              
            | WhileStmt (exp, stmtlist) -> 
              let new_exp = type_check_expression exp current_cname type_method_env class_decl_list const_all_super_classes in 
                let (_, new_type) = get_typed_exp(new_exp) in 
                  if compare_mOOL_types new_type BoolT then(
                    (* Get rest of statements in while loop*)
                    let sub_ret_type, sub_stmts = helper ret_type stmtlist [] in 
                      let new_list =  return_list@[WhileStmt(new_exp, sub_stmts)] in 
                        helper sub_ret_type tail new_list)
                  else
                    failwith ("Type-check error: " ^ string_of_mOOL_expr exp)

            | ReadStmt (var_id) -> 
              let (s,t,i) = find_typed_var_in_env var_id type_method_env in
                (match t with 
                ObjectT somename -> failwith("Type-check error: Can't read this object")
                | _ -> let new_list = return_list@[ReadStmt(TypedVarId(s,t,i))] in helper t tail new_list)
            
            | PrintStmt (exp) -> 
              let new_exp = type_check_expression exp current_cname type_method_env class_decl_list const_all_super_classes in
                let (new_exp, new_type) = get_typed_exp(new_exp) in 
                  (match new_type with 
                  ObjectT somename -> failwith("Type-check error: Can't print this object")
                  | _ -> let new_list = return_list@[PrintStmt(new_exp)] in helper new_type tail new_list)

            | AssignStmt (var_id, exp) ->   
              (* Find the variable to assign in environment *)
              let (s,t,i) = find_typed_var_in_env var_id type_method_env in
                (* Type check the expression after *)
                let new_exp = type_check_expression exp current_cname type_method_env class_decl_list const_all_super_classes in
                  (* Just get the expression and type associated with the typed exp*)
                  let (new_exp2, new_type) = get_typed_exp(new_exp) in
                    (*If they are the same type ok*)
                    if(compare_mOOL_types t new_type || is_super_class t const_all_super_classes) then
                      (* Add the new successful statement to the list and recur *)
                      let new_list =  return_list@[AssignStmt (TypedVarId (s,t,i), new_exp)] in helper t tail new_list
                    else failwith("Type-check error: Types " ^ string_of_mOOL_type t ^ " and " ^
                         string_of_mOOL_type new_type ^ " can't be assigned in method " ^
                        string_of_var_id curr_method.mOOLid) 
           
            | AssignFieldStmt(exp, exp2) -> 
              let new_exp = type_check_expression exp current_cname type_method_env class_decl_list const_all_super_classes in
              let new_exp2 = type_check_expression exp2 current_cname type_method_env class_decl_list const_all_super_classes in
              let (new_exp, new_type) = get_typed_exp(new_exp) in 
              let (new_exp2, new_type2) = get_typed_exp(new_exp2) in 
            
              if(compare_mOOL_types new_type new_type2) then 
                let new_list = return_list@[AssignFieldStmt(new_exp, new_exp2)] in helper new_type tail new_list
              else
                failwith("Type-check error: Types " ^ string_of_mOOL_type new_type ^ " and " ^
                         string_of_mOOL_type new_type2 ^ " can't be assigned in method " ^
                        string_of_var_id curr_method.mOOLid) 
            | MdCallStmt (exp) -> 
                let new_exp = type_check_expression exp current_cname type_method_env class_decl_list const_all_super_classes in
                let _, new_type = get_typed_exp(new_exp) in   
                (match new_type with
                | Unknown -> failwith "Type-check error in MdCallStmt "
                | _ -> let new_list = return_list@[MdCallStmt(new_exp)] in helper new_type tail new_list)
            | ReturnStmt (exp) -> 
              let new_exp = type_check_expression exp current_cname type_method_env class_decl_list const_all_super_classes in 
                let _, new_type = get_typed_exp (new_exp) in 
                  let new_list = return_list@[ReturnStmt(new_exp)] in helper new_type tail new_list
            | ReturnVoidStmt -> let new_list = return_list@[ReturnVoidStmt] in helper VoidT tail new_list
          end
        | [] -> ret_type, return_list
      in helper Unknown curr_method.stmts return_statements

(* Type check a method declaration *)
let type_check_mthd_decl cname curr_method class_env class_decl_list const_all_super_classes count ismain: md_decl= 
  let method_env = List.append curr_method.params curr_method.localvars in (* Get all local vars and paramters *)
    check_if_var_decl_list_is_ok method_env class_decl_list; (* Check so that the vars and parameters are valid, we do not want to create classes that do not exist*)
    let type_method_env = create_local_env method_env 2 in  (* Create the local environment associated with scope number 1*)
       let ret_type, new_stmts = type_check_statements type_method_env class_env curr_method cname class_decl_list const_all_super_classes in (* Check all the statements in method *)
          if(compare_mOOL_types curr_method.rettype ret_type) then
            if ismain == false then {curr_method with stmts = new_stmts; ir3id = (SimpleVarId("_" ^ cname ^ "_" ^ string_of_int !count));}
            else {curr_method with stmts = new_stmts; }
          else failwith("Type-check error: Method " ^ string_of_var_id curr_method.mOOLid ^ " has the wrong return-type, expected : " ^ string_of_mOOL_type ret_type)

(* Type checks main class which has only one method *)
let type_check_main_class class_main class_decl_list : class_main = 
  let (cname, mth) = class_main in 
    let new_method_decl = type_check_mthd_decl cname mth [] class_decl_list [] (ref 0) true in 
      (cname, new_method_decl)

(* Returns a list with all the Simple variables in environment, if duplicates is detected, dont add them *)
let rec shadow_variables main_attr_list attr_list_to_add : attr_decl list = 
  
      match attr_list_to_add with
      (modf, (m_type, var_id)) as curr_attr :: tail ->
        (match modf with 
        Private -> shadow_variables main_attr_list tail  (* Ignoring this one *)
        | Public -> 
          (match var_id with (* Only gonna match simple var id's here *)
          SimpleVarId id -> 
            (*print_string("\n ALL ATTR \n");
            print_attr(main_attr_list);
            print_string("\n LOOKING FOR " ^ string_of_var_id var_id);*)
            if(check_if_attr_id_exists main_attr_list var_id) then shadow_variables main_attr_list tail 
            else let new_list = main_attr_list@[curr_attr] in shadow_variables new_list tail 
            | TypedVarId id -> failwith("Type-check error: variable is already type-checked"))
        )
      | [] -> main_attr_list
    

let rec get_attributes_from_ext_class ext_class class_decl_list other_attributes all_super_classes const_class_decl_list: class_decl list * attr_decl list = 
    match class_decl_list with
    (cname,ext_c2,more_attr,_) as new_class :: tail ->
      if String.compare ext_class cname == 0 then (* Found an extended class, get new variables except shadow duplicates*) 
        (let new_list = shadow_variables other_attributes more_attr in (*TODO : HERE WE NEED TO DEFINE A FUNCTION, NOT JUST APPEND THE VALUES -> SHADOWING*)
          (match ext_c2 with
          Some even_more_extended -> print_string("and " ^ cname ^ " more ext");let new_all_super_classes = all_super_classes @ [new_class] in get_attributes_from_ext_class even_more_extended const_class_decl_list new_list new_all_super_classes const_class_decl_list
          | None -> let new_all_super_classes = all_super_classes @ [new_class] in new_all_super_classes, new_list ))
      else get_attributes_from_ext_class ext_class tail other_attributes all_super_classes const_class_decl_list
    | [] -> all_super_classes, other_attributes

(* Type check a method declaration *)
let rec type_check_mthd_decl_list cname class_env method_list class_decl_list const_all_super_classes: md_decl list= 
  let return_list = [] in
    let count = ref 0 in (*for ir3id*)
    let rec helper cname class_env method_list class_decl_list the_list const_all_super_classes =
      match method_list with
      head :: tail -> 
        let type_check_mth = type_check_mthd_decl cname head class_env class_decl_list const_all_super_classes count false in
          incr count; 
          let new_list = the_list@[type_check_mth] in helper cname class_env tail class_decl_list new_list const_all_super_classes
      | [] -> the_list
    in helper cname class_env method_list class_decl_list return_list const_all_super_classes

(* Type check a new class *)
let type_check_class_decl class_decl class_decl_list: class_decl =
  (* Get list with class attributes*)
  let (cname, extends_class, attr_list, md_list) = class_decl in
    match extends_class with
    Some ext_class -> 
      (* Get other attributes of all parent classes *)

      let (all_super_classes, other_attributes) = get_attributes_from_ext_class ext_class class_decl_list attr_list [] class_decl_list in

       (*  print_string(" \n ALL SUPER CLASSES OF " ^ cname);
        print_classes all_super_classes;
        print_string("\n------------------\n"); *)
        (* Merge other attributes with current attributes*)
        (*let all_attributes = List.append attr_list other_attributes in *)
          (* Create class environment *)
          let class_environment = create_class_env other_attributes 1 in
            (* Type check all methods in class *)
            let new_mth_decl_list = type_check_mthd_decl_list cname class_environment md_list class_decl_list all_super_classes in
              (cname, Some ext_class, attr_list, new_mth_decl_list)
    | None -> 
      let class_environment = create_class_env attr_list 1 in
        let new_mth_decl_list = type_check_mthd_decl_list cname class_environment md_list class_decl_list [] in
          (cname, None, attr_list, new_mth_decl_list)

let type_check_class_decl_list class_decl_list : class_decl list = 
  let return_class_decl_list = [] in 
    let rec helper curr_class_list list_to_return class_decl_list =
      match curr_class_list with
      head :: tail -> 
        let new_class_decl = type_check_class_decl head class_decl_list in 
          let new_list = list_to_return@[new_class_decl] in helper tail new_list class_decl_list
      | [] -> list_to_return
    in helper class_decl_list return_class_decl_list class_decl_list

(* TypeCheck a MOOL Program. 
   Return a new MOOL Program where expressions are annotated with types
 *)
let type_check_mOOL_program  
  (p:mOOL_program) : mOOL_program = 
    let (class_main, class_list) = p in 

      (* Static checking in program *)
      check_for_unique_class_name class_list;
      check_inheritance class_list [];
      check_for_distinct_class_attr class_list; 
      check_for_distinct_parameters_method class_list;
      check_for_distinct_local_vars_method class_list;
      check_for_invalid_methods class_list;
      
      (* Recursive type checking starts here*)
      let new_class_main = type_check_main_class class_main class_list in
        let new_class_decl_list = type_check_class_decl_list class_list in 
          (new_class_main, new_class_decl_list)
