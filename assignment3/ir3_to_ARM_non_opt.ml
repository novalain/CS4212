(* ===================================================== *)
(* ============== CS4212 Compiler Design ============== *)
(*   Transformation to intermediary representation IR3   *)
(* ============ TEAM MOANG================================ *)
(* ===================================================== *)

open Arm_structs
open Ir3mOOL_structs
open MOOL_structs

(* node stored for each variable in the offset_tbl type. it stores the position
   in the stack with the correct offset from fp and indicates whether this is the
   last local/temp on the stack before the callee frame/activation record. *)   
type offset_node = {pos:int; last:bool}
(* hash table used to store the variables offset for addressing local and
   temporaries variables within the stack. parameters are handled through the
   registers a1-a4. a1 used as the return register. case where there is more than
   4 parameters is handled by storing variables within the stack. this is done
   after space allocated to locals/temp. *)
type offset_tbl = (id3,offset_node) Hashtbl.t

(* print the key:value pairs of the offset hash table *)
let print_offset_tbl (tbl:offset_tbl) (md_name:id3) =
    let _ = print_endline(md_name ^ " hash table of locals/parameters") in
    Hashtbl.iter (fun key value -> print_endline (key^"\t"^(string_of_int value.pos))) tbl;
    print_endline("\n")

(* function that return the offset from the fp. need to increment of 28 *)
let get_offset_var (key:id3) (tbl:offset_tbl) : int =
    let node = Hashtbl.find tbl key in node.pos

(* find the type of a variable *)
let rec get_var_type (id3_var:id3) (md_name:string) (locals_params:var_decl3 list) : string =
    match locals_params with
    | (id3_type,id3_id) :: tail ->
        if (String.compare id3_id id3_var==0)
        then string_of_mOOL_type id3_type
        else get_var_type id3_var md_name tail
    | [] -> failwith("Variable " ^ id3_var ^ " hasn't been defined in method " ^ md_name) 

(* return the last local/temporary/param offset in the stack. get the stack offset
   from fp to sp *)
let get_stack_offset (tbl:offset_tbl) : int =
    let node = Hashtbl.find tbl "last" in node.pos
 
(* add an element to the offset table *)
let set_offset_var (key:id3) (value:offset_node) (tbl:offset_tbl) (last:bool) =
    Hashtbl.add tbl key value;
    if last
    then Hashtbl.add tbl "last" value
    else ()

(* creates the offset hash table for the locals and parameters and the
   corresponding load store machine instructions corresponding.
   our design is as follows: when a function is called, all its locals/temp and
   params are allocated a space in the stack *)
let create_offset_tbl (md:md_decl3) (size:int) =
    let lt = Hashtbl.create size in
    let rec helper (n:int) (vars:var_decl3 list) : offset_tbl =
        match vars with
        | (ir3_type_0,id3_0) :: vdecl3_tail ->
            (* check whether id3_0 is the last variable to which a location need
               to be reserved *)
            let last = List.length vdecl3_tail==0 in
            set_offset_var id3_0 {pos=4*n; last=true} lt last;
            helper (n+1) vdecl3_tail
        | [] -> lt
    in helper 1 (md.params3@md.localvars3)

(* only for debugging *)
let debug_arm_instr arm_instr = 
    print_string("\n ** Arm instr: " ^ string_of_arm_instr(arm_instr))

let labelcount = ref 0 
(* returns a fresh label *)
let fresh_label () : string = 
    (labelcount:=!labelcount+1; "L" ^ (string_of_int (!labelcount-1)))

(* return the attributes of a class *)
let get_attrs_class (name_class:string) (classes,_,_:ir3_program) : (var_decl3 list) =
    let rec helper (class_list:cdata3 list) = 
        match class_list with
        | (cname, attr_list) :: class_tail ->
            if (String.compare cname name_class == 0)
            then attr_list
            else helper class_tail
        | [] -> failwith("Objects of type " ^ name_class ^ " hasn't been defined.")
    in helper classes

(* return the offset of the field of an object *)
let get_offset_var_field
    (id3_var:id3) (id3_var_attr:id3) (md:md_decl3) (tbl:offset_tbl) (ir3prog:ir3_program) : int =
    (* we get the type of the id3_var so that we can access its attributes *)
    let id3_type_name = get_var_type id3_var md.id3 (md.localvars3 @ md.params3) in
    let cls_attrs = get_attrs_class id3_type_name ir3prog in
    (* n is the position in which the attribute appears in the class where it is defined *)
    let rec helper (attr_searched:id3) (class_attrs:var_decl3 list) (n:int) : int =
        begin
            match class_attrs with
            | (attr_type,attr_var) :: tail ->
                if (String.compare attr_searched attr_var==0)
                then n
                else helper attr_searched tail (n+1)
            | [] -> failwith("Attribute " ^ attr_searched ^ " hasn't been found in the "
                             ^ "class " ^ id3_type_name)
        end
    in (helper id3_var_attr cls_attrs 1) * 4

(* returns the size of the memory to allocate for a dynamic object (ObjectCreate3) *)
let get_mem_size_ObjectCreate3
    (name_class:string) (attrs_list:var_decl3 list) (ir3prog:ir3_program) : int =
    let rec helper class_attributes (size:int) =
        match class_attributes with
        | (attr_type,attr_id3) :: tail ->
            begin
                match attr_type with
                | ObjectT cname ->
                    let cls_attrs = get_attrs_class cname ir3prog in
                    let sub_cls_attrs_size = helper cls_attrs 0 in
                    helper tail (size+sub_cls_attrs_size)
                (* here attribute is of type primitive *)
                | _ -> helper tail (size+4)
            end
        | [] -> size
    (* start iteration with an empty list *)
    in helper attrs_list 0

let number_op (i:int) : string =
    "#" ^ string_of_int i

let load_idc3 (var:idc3) (register:reg) (lp:offset_tbl) : arm_program =
    match var with
    | IntLiteral3 idc3_int ->
        MOV ("", false, register, ImmedOp (number_op idc3_int)) :: []
    | BoolLiteral3 idc3_bool ->
        begin
            match idc3_bool with
                | false -> MOV ("", false, register, ImmedOp (number_op 0)) :: []
                | true -> MOV ("", false, register, ImmedOp (number_op 1)) :: []             
         end 
    (* a string behaves differently since it is referenced through a label, then
       we do not need to implement it *)
    (* | StringLiteral3 idc3_string -> *)
    | Var3 idc3_id3 ->
        let offset = get_offset_var idc3_id3 lp in
        LDR ("", "", register, RegPreIndexed ("fp", -offset, false)) :: []
    | _ -> failwith("StringLiteral3 cannot be directly loaded into a register.
                    Only its label is load such as ldr a1,=L1 where L1 is the
                    label pointing to the string in the .data header.")

(* return the exit label for a function *)
let get_exit_name (name:id3) : string =
    "." ^ name ^ "_exit"

(* generates the label for a function *)
let convert_md_name (name:id3) : string =
    "\n" ^ name ^ ":"

(* create a contant variable from a string *)
let create_constant_string (str:string) : arm_program =
    let lbl = fresh_label() in
    Label lbl :: PseudoInstr (".asciz "^"\""^str^"\\n\"") :: []

(* convert IR3 Expressions to ARM Insstructions *)
let convert_ir3_expr
    (exp:ir3_exp) (md:md_decl3) (lp:offset_tbl)
    (ir3prog:ir3_program) : arm_program * arm_program =
    match exp with
    | BinaryExp3 (operator, idc3_1, idc3_2) ->
        (* idc3_1 (resp. idc3_2) are stored in v1 (resp. v2) *)
        let load_instr1 = load_idc3 idc3_1 "v2" lp in
        let load_instr2 = load_idc3 idc3_2 "v3" lp in
        (* debug_arm_instr(instr1); debug_arm_instr(instr2); *)
        begin
            (* binary expresssions handled: arithmetics, boolean, relational *)
            match operator with
            | AritmeticOp op ->
                (* for each operation, the operands are loaded (v2 and v3),
                   value is computed and result is kept in register v1 *)
                let arm_op_instructions =
                    begin
                        match op with
                        | "+" ->
                            [ADD ("", false, "v1", "v2", (RegOp "v3"))]
                        | "-" ->
                            [SUB ("", false, "v1", "v2", (RegOp "v3"))]
                        | "*" ->
                            [MUL ("", false, "v1", "v2", "v3")]
                        | _ -> failwith("Unknown operator or trying to divide.
                                        Not allowed.")
                    end
                in
                [], load_instr1 @ load_instr2 @ arm_op_instructions
           (* operations handled: < > <= >= == != *)
           | RelationalOp op ->
                (* value in v3 is substracted from the value in v2 such that
                   result = v3 - v2. here type checking gives that v2/v3 are int *)
                let comparative_inst = [CMP ("", "v2", (RegOp "v3"))] in
                let arm_op_instructions =
                    begin
                        (* two MOV instructions are used to translate the if else
                           statement. the first inst (condition verified) gets
                           program into the if, the second into the else (condition
                           not verified). See link for further explanations:
                           http://www.davespace.co.uk/arm/introduction-to-arm/conditional.html *)
                        match op with
                        | "==" ->
                            MOV ("eq", false, "v1", ImmedOp (number_op 1)) ::
                            MOV ("ne", false, "v1", ImmedOp (number_op 0)) :: []                         
                        | "!=" ->
                            MOV ("ne", false, "v1", ImmedOp (number_op 0)) ::
                            MOV ("eq", false, "v1", ImmedOp (number_op 1)) :: []
                        | ">" ->
                            MOV ("gt", false, "v1", ImmedOp (number_op 1)) ::
                            MOV ("lt", false, "v1", ImmedOp (number_op 0)) :: []
                        | ">=" ->
                            MOV ("ge", false, "v1", ImmedOp (number_op 1)) ::
                            MOV ("lt", false, "v1", ImmedOp (number_op 0)) :: []
                        | "<" ->
                            MOV ("lt", false, "v1", ImmedOp (number_op 1)) ::
                            MOV ("ge", false, "v1", ImmedOp (number_op 0)) :: []
                        | "<=" ->
                            MOV ("le", false, "v1", ImmedOp (number_op 1)) ::
                            MOV ("gt", false, "v1", ImmedOp (number_op 0)) :: []
                        | _ -> failwith "Unknown Relational Operation."
                    end
                in [], load_instr1 @ load_instr2 @ comparative_inst @ arm_op_instructions
           (* operations handled: ||, && *)
           | BooleanOp op -> 
                let arm_op_instructions =
                    begin
                        (* as a reminder, idc3_1/idc3_2 are stored in v2/v3. result
                           is retrieved in v1 *)
                        match op with
                        | "&&" -> 
                            [AND ("", false, "v1", "v2", RegOp "v3")]
                        | "||" -> 
                            [ORR ("", false, "v1", "v2", RegOp "v3")]
                        | _ -> failwith "Unknown Boolean Operation."
                    end
                in [], load_instr1 @ load_instr2 @ arm_op_instructions
            | _ -> failwith "Unknown Binary Operator."
        end
   | UnaryExp3 (operator, idc3_var) ->
        (* load variable into register v2 *)
        let load_instr = load_idc3 idc3_var "v2" lp in
        let arm_op_instructions =
            begin
                match operator with
                | UnaryOp op ->
                    begin 
                        match op with 
                        | "!" -> 
                            (* I don't get it. Maybe a CMP then a MOVEQ, MOVNE would be better? *)
                            (* Reverse-substract the values. Just invert and them and store in res*)
                            RSB("", false, "v1", "v2", ImmedOp(number_op 1))
                        | "-" -> 
                            RSB("", false, "v1", "v2", ImmedOp(number_op 0))
                        | _ -> failwith "Unkown Unary Operator."
                    end
                (* operator has type mOOL_op, then need to catch all others operators
                   since here we focus only on the UnaryOp *)
                | _ -> failwith "Applying a non unary operator to an UnaryExp."
            end
        in [], load_instr @ arm_op_instructions :: []
    | FieldAccess3 (idc3_var1, idc3_var2) ->
        (* load the value of id3_var2 into a register *)
        let offset_idc3_var1 = get_offset_var idc3_var1 lp in
        let offset_idc3_var2 = get_offset_var_field idc3_var1 idc3_var2 md lp ir3prog in
        [],
        LDR ("", "", "v2", (RegPreIndexed ("fp", -offset_idc3_var1 , false))) ::
        LDR ("", "", "v1", (RegPreIndexed ("v2", -offset_idc3_var2, false))) :: []
    | Idc3Expr idc3_var ->
        let (data_instr,machine_instr) =
            begin
                match idc3_var with
                | IntLiteral3 v ->
                    (* a value is directly moved to the register. default register used is v1 *)
                    [], [MOV ("", false, "v1", ImmedOp (number_op v))]
                | BoolLiteral3 b ->
                    begin
                        (* false has value 0 and true has value 1 *)
                        match b with
                        | false -> [], [MOV ("", false, "v1", ImmedOp (number_op 0))]
                        | true  -> [], [MOV ("", false, "v1", ImmedOp (number_op 1))]
                    end
                | StringLiteral3 v ->
                    (* strings are handled in data instructions and referred with labels *)
                    (* we create this label when printstatement occurs instead,
                    otherwise need to add labelnumber to hashtable or something....*)
                    [], []
                | Var3 var3 ->
                    let offset = get_offset_var var3 lp in
                    [], [LDR ("", "", "v1", RegPreIndexed ("fp", -offset, false))]
            end
        in (data_instr,machine_instr)
    | MdCall3 (id3_var, idc3_list) -> 
        let machine_instr = 
            let count = ref 0 in
            (* go through all the function arguments *)
            let rec helper args machine_instr =
                match args with
                | arg :: arg_tail ->
                    incr count;
                    if (!count < 5)
                    (* load the argument into an ai register *)
                    then let load_instr = load_idc3 arg ("a" ^ string_of_int(!count)) lp in
                    helper arg_tail (machine_instr @ load_instr)
                    else
                        (* all ai registers are used, we need to add other arguments
                           into the stack and update the stack pointer for arguements
                           space allocation *)
                        let extra_args = !count - 4 in
                        let new_machine_instr =
                            machine_instr @
                            [SUB("", false, "sp", "sp", ImmedOp(number_op(extra_args * 4)))] in
                        helper arg_tail new_machine_instr
                | [] -> machine_instr
            in helper idc3_list []
        in
        (* We have to move returnvalue a1 back to v1 *)
        let new_list = machine_instr @
                       [BL("", id3_var ^ "(PLT)")] @
                       [MOV("", false, "v1", (RegOp "a1"))] in
        ([],new_list)
    | ObjectCreate3 cname ->
        (* objects will reside in dynamic memory (see A.3.4 exercise in paper) *)
        (* get the class attributes of the current class *)
        let cls_attrs = get_attrs_class cname ir3prog in
        let mem_size = 4 * (List.length cls_attrs) in
        (* let mem_size = get_mem_size_ObjectCreate3 cname cls_attrs ir3prog in  *)
        [], MOV("", false, "a1", ImmedOp(number_op mem_size)) ::
            BL("", "_Znwj(PLT)") ::
            (* the address where the dynamic object is stored is returned in a1 *)
            MOV("", false, "v1", RegOp "a1") :: []

(* convert an ir3 statement to ARM instructions *)
let convert_ir3_stmt
    (stmt:ir3_stmt) (md:md_decl3) (lp:offset_tbl)
    (ir3prog:ir3_program) : arm_program * arm_program = 
    match stmt with
    | Label3 label ->
        (* solution based on the testcases given in testcases/ folder *)
        [], [PseudoInstr ("\n." ^ string_of_int label ^ ":")]
    | IfStmt3 (if_exp, label) ->
        let (data_instr,machine_instr) = convert_ir3_expr if_exp md lp ir3prog in
        data_instr,
        (* in convert_ir3_expr, value 1 (~true) is in v1 or 0 (~false) according
           to the BinaryExp. now, program branches to label if condition is
           satisfied, otherwise keep processing instructions. *)
        machine_instr @ [CMP("", "v1", ImmedOp (number_op 1));
                         B("eq", "." ^ string_of_int label)]
    | GoTo3 label -> 
        (* instruction B is used since such jump is used in loops. BL is used when
           a method is called so that the address of the next instruction is copied
           into the Link Register lr, used for return sequence. *)
        [], [B ("", "." ^ string_of_int label)]
    | PrintStmt3 idc3_var ->
        (* we need to specify a Label, load variable into register and then print
        to registers address *)
        let new_label = fresh_label() in 
        begin
            match idc3_var with 
            | IntLiteral3 _int ->
                let new_data_instr =
                    Label new_label :: 
                    PseudoInstr (".asciz \"%i\\n\"") :: [] in
                (* loading is done in a1 since this register is used for argument
                   passing. *)
                let new_machine_instr =
                    LDR("", "", "a1", Reg ("=" ^ new_label)) ::
                    MOV("", false, "a2", ImmedOp (number_op _int)) ::
                    BL("", "printf(PLT)") :: [] in 
                new_data_instr, new_machine_instr
            | BoolLiteral3  _bool ->
                let new_data_instr =
                    Label new_label :: 
                    PseudoInstr (".asciz \"" ^ string_of_bool _bool ^ "\\n\"") :: [] in
                let new_machine_instr =
                    LDR("", "", "a1", Reg ("=" ^ new_label)) ::
                    BL("", "printf(PLT)") :: [] in 
                new_data_instr, new_machine_instr
            | StringLiteral3  _string ->
                let new_data_instr =
                    Label new_label :: 
                    PseudoInstr (".asciz \"" ^ _string ^ "\\n\"") :: [] in
                let new_machine_instr =
                    LDR("", "", "a1", Reg ("=" ^ new_label)) ::
                    BL("", "printf(PLT)") :: [] in 
                new_data_instr, new_machine_instr
            | Var3 _id3 ->
                let new_data_instr =
                    Label new_label :: 
                    PseudoInstr (".asciz \"%i\\n\"") :: [] in
                let new_machine_instr =
                    LDR("", "", "a1", Reg ("=" ^ new_label)) ::
                    (* _id3 is loaded from the stack into the temporary a2 *)
                    LDR("", "", "a2", RegPreIndexed ("fp", -get_offset_var _id3 lp, false)) ::
                    BL("", "printf(PLT)") :: [] in 
                new_data_instr, new_machine_instr 
        end
    | AssignStmt3 (id_lv, exp) ->
        let (data_instr,machine_instr) = convert_ir3_expr exp md lp ir3prog in
        let offset = get_offset_var id_lv lp in
        let new_machine_instr = machine_instr @
                                [STR ("", "", "v1", RegPreIndexed ("fp", -offset, false))] in
        (data_instr,new_machine_instr)
    (* | AssignDeclStmt3 (id_lv_type, id_lv, exp) -> *)
    | AssignFieldStmt3 (exp_1, exp_2) ->
        (* exp_1 is an ir3_expr. We first need to check if this is a FieldAcess exp
           otherwise we throw an error. *)
        let (obj_id3_var,obj_idc3_attr) =
            begin
                match exp_1 with
                | FieldAccess3 (v1, v2) -> (v1,v2)
                | _ -> failwith("An AssignFieldStmt3 is not used with a FieldAccess3 expression.")
            end
        in (* in real programming languages, righ hand can be a method call, an idc3,
        a FieldAcces.. *)
        let load_instr_assign =
            begin
                match exp_2 with
                (* load exp_2 according to its type into v2, i.e. the assigned variable *)
                | Idc3Expr idc3_var -> (load_idc3 idc3_var "v1" lp)
                | _ -> failwith("AssignFieldStmt3 should be used with a idc3 type in its
                                right hand. Assumption done for design simplicity.")
            end
        in
        let offset_obj_id3_var = get_offset_var obj_id3_var lp in
        let offset_obj_idc3_attr = get_offset_var_field obj_id3_var obj_idc3_attr md lp ir3prog in
        [],
        (* the value to store is kept in v1 *)
        load_instr_assign @
        LDR ("", "", "v2", (RegPreIndexed ("fp", -offset_obj_id3_var , false))) ::
        STR ("", "", "v1", (RegPreIndexed ("v2", -offset_obj_idc3_attr, false))) :: []
    | MdCallStmt3 exp ->
        let (data_instr,machine_instr) = convert_ir3_expr exp md lp ir3prog in
        (data_instr,machine_instr)
    (* such statement is only used in the class methods other than Main. Indeed,
       main method returns everytime void. *)
    | ReturnStmt3 id3_var ->
        (* return value is stored in register a1 and automatically returned to caller. *)
        let offset = get_offset_var id3_var lp in
        let exit_callee = LDR ("", "", "a1", RegPreIndexed ("fp", -offset, false)) ::
                          B ("", get_exit_name md.id3) :: [] in
        (* 28 is the correct value since sp need to point just before the saved status.
           also, using ldmfd implcitly copies lr into pc in order to call the previous
           procedure. *)
        [], exit_callee
    (* such statement return from the callee to the caller frame by restoring status *)
    | ReturnVoidStmt3 ->
        [], [B ("", get_exit_name md.id3)]
    | _ -> failwith "Trying to use ReadStmt3 which is not implemented in the backend."

(* convert a method's statements into ARM instructions *)
let convert_ir3_stmt_list
    (md:md_decl3) (lp:offset_tbl) (ir3prog:ir3_program) =
    let rec helper (stmts:ir3_stmt list) data_list mac_list =
        match stmts with
        | st :: tail ->
            let (new_data_list,new_mac_list) = convert_ir3_stmt st md lp ir3prog in 
            helper tail (data_list@new_data_list) (mac_list@new_mac_list)
        | [] -> (data_list,mac_list)
    in
    helper md.ir3stmts [] []

(* generates the data and the machine instructions for a ir3 method *)
let convert_md3
    (md:md_decl3) (ir3prog:ir3_program) : arm_program * arm_program =
    (* create locals/temporaries of the method md. lp stands for locals/parameters *)
    let lt_offset_tbl = create_offset_tbl md 100 in
    (* used for debug purposes *)
    (* let _ = print_offset_tbl lt_offset_tbl md.id3 in *)
    let (data_instr,mac_instr) = convert_ir3_stmt_list md lt_offset_tbl ir3prog in
    let pop_saved_status = Label (get_exit_name md.id3) ::
                           SUB ("", false, "sp", "fp", ImmedOp (number_op 28)) ::
                           LDMFD ["v1";"v2";"v3";"v4";"v5";"fp";"pc"] :: [] in
    data_instr,
    PseudoInstr (convert_md_name md.id3) ::
    (* lr (link register) stores the address of the caller for the return calling sequence *)
    STMFD ["v1";"v2";"v3";"v4";"v5";"fp";"lr"] ::
    (* each register is 4-bytes. then 7*4 = 28
       fp is updated such that it points to the previous activation record (or frame record) *)
    ADD ("", false, "fp", "sp", ImmedOp (number_op 28)) ::
    (* sp is updated to point to the top of the stack. then from fp we need to add
       the place for locals/parameters variables. after the fp, which is the location
       of the last register store in the stack, place is reserved for local variables and
       parameters. according to their number, the sp is set accordingly. locals/params
       can then be accessed through an offset from sp by preindexing/postindexing
       whether we want to rewrite the value or not *)
    SUB ("", false, "sp", "fp", ImmedOp (number_op (28 + get_stack_offset lt_offset_tbl))) ::
    (* TODO: local/parameters machine instructions *)
    (* machines instructions are set *)
    mac_instr @
    (* setting the pop_saved_status here avoid duplicating the .exit_name_function
       when there are IfStmt. Indeed, in such a stmt, a return stmt is used in then
       AND else, thus generating two exiting labels for the current function. Now
       in such case, only the jump are generated and the exit is handled statically here. *)
    pop_saved_status

(* entry point function *)
let ir3_program_to_ARM (ir3prog:ir3_program) : arm_program =
    let (cdata,main_md,md_list) = ir3prog in
    let mds = (main_md :: md_list) in
    let (data_instructions,machine_instructions) =
        let rec helper di mi methods =
            match methods with
            | md :: mds_tail ->
                let (md_di,md_mi) = convert_md3 md ir3prog in
                let (di_new,mi_new) = (di@md_di,mi@md_mi) in
                helper di_new mi_new mds_tail
            | [] -> (di,mi)
        in
        helper [] [] mds in
    [PseudoInstr ".data"] @
    data_instructions @
    [PseudoInstr "\n\t.text"; PseudoInstr ".global main";
     PseudoInstr ".type main, %function"] @
    machine_instructions

(* some documentation:
   The STMFD instruction pushes registers onto the stacks, updating the sp.
   ST stands for store register (similar to STR)
   M  stands for multiple
   F  stands for a full stack, which means sp points to the last item in the stack.
      (E is for a sp which points to the first unused address in the stack)
   D   stands for ascending. Stack grows in the direction of smaller addresses
       (A is the opposite, i.e. the ascending addresses)
   stmdf sp!,{...} here sp! means that the stack pointer is automatically updated *)

(* see page 60 in ARM System Developer Guide for further information
    link register lr with a return address. It performs a subroutine call.
    This example shows a simple fragment of code that branches to a subroutine
    using the BL instruction. To return from a subroutine, you copy the link
    register to the pc. *)
