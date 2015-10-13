
(* ==================================================== *)
(* ============== CS4212 Compiler Design ============== *)
(*   	       Static Check of mOOL programs            *)
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


let get_class_names lst = 
  List.map (fun (a,b,c,d) -> a) lst

let get_scope_variables lst = 
  List.map (fun (a,b) -> (match b with SimpleVarId str -> str) ) lst

let get_all_parents lst = 
 List.map ( fun (a,b,c,d) -> (match b with None -> "" | Some str -> str)) lst

let rec check_for_unique_class_name
  (class_decl_list:class_decl list) =
  match class_decl_list with
  (cname,parent,_,_) :: tail -> (* Or head and (cname,_,_,_) and match head *)
      if (List.exists (fun (n) ->
            String.compare cname n == 0)
          (get_class_names tail)) (* Checks if cname exists in the list of all classes - cname*)
        then Some cname (*there are duplicates.*)
      else check_for_unique_class_name tail
  | [] -> None

(* For debugging a list *)
let print_list lst = 
  print_string(string_of_list(lst) (fun a -> a) ",")

(*)
let rec check_for_inheritance_cycles
  (class_decl_list : class_decl list) (all_classes: class_name list) (all_parents : class_name list) = 
  match class_decl_list with  
  (cname,parent,_,_) :: tail ->
      (match parent with
      Some pa ->
        if ( String.compare cname pa == 0 )
          then failwith("Class " ^ pa ^ "extends itself")
        else (let rec helper (rest_of_list : class_decl list) (looped_class : string) = 
          (match rest_of_list with
          (cname2, parent, _, _) :: tail2 -> 
            (match(parent) with
              Some pa2 -> 
                if (String.compare cname2 looped_class == 0) then begin
                  if (String.compare cname2 pa2 == 0) then failwith "class inheritance"
                  else helper tail2 pa2
                end
                else helper tail2 looped_class
              | None -> helper tail2 looped_class)
          | [] -> check_for_inheritance_cycles tail all_classes all_parents) (* utan denna funkar d *)
          in helper tail pa)
      | None -> check_for_inheritance_cycles tail all_classes all_parents )
  | [] -> None*)

(* Class can only be extended AFTER it has been declared *)
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
  | [] -> None

(*
let add_to_class (a_list : string list) = 
  let new_list = "MDEO" :: a_list; in print_list(new_list)*)
(*aaaaaaa
a aaaaaa*)

let rec check_for_distinct_scopes (class_decl_list:class_decl list) = 
  match class_decl_list with 
    (_,_,attribute_list,_) :: tail ->
      let rec helper list_of_vars_in_class prev_decl_vars = 
        match list_of_vars_in_class with 
          (_, var_decl) :: tail2 -> 
            (match var_decl with 
            (_, var_id) -> 
              if( let vid = string_of_var_id var_id in List.exists (fun (n) -> String.compare vid n == 0) (prev_decl_vars) ) then
                failwith(string_of_var_id(var_id) ^ " is not a unique name in class")
              else let new_list = string_of_var_id var_id :: prev_decl_vars in helper tail2 new_list)
          | [] -> check_for_distinct_scopes tail
      in helper attribute_list []
    | [] -> ()

(* TypeCheck a MOOL Program. 
   Return a new MOOL Program where expressions are annotated with types
 *)
let type_check_mOOL_program  
  (p:mOOL_program) : mOOL_program = 
    match p with
    | (main_class, s) ->
      match (check_for_unique_class_name s) with
      Some a -> failwith ("Duplicated names, " ^ a ^ " is a duplicate")
      | None -> 
        match(check_inheritance s []) with
        Some err -> failwith err
        | None -> check_for_distinct_scopes s; p 

        (*match( check_for_inheritance_cycles s (get_class_names(s)) (get_all_parents(s))) with
        Some a -> failwith(a); 
        | None -> print_string("all ok"); p*)
        




     
