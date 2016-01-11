(* ===================================================== *)
(* ============== CS4212 Compiler Design =============== *)
(*         Transformation IR3 to basic blocks rep        *)
(* ==================== TEAM MOANG ===================== *)
(* ===================================================== *)

open MOOL_structs
open Ir3mOOL_structs

(* define the set for the liveness (in/out/def/use) containing id3 (=string)
   variables *)
module Id3_set = Set.Make(String);;

type id3_set = Id3_set.t;;

(* variables explanation for the statement node:
        id_stmt:        unique identifier of a stmt within a babl
        stmt:           ir3_stmt as in the ir3_program
        succ:           sucessor stmt. -2 if current stmt is the last
        pred:           predecessor stmt. -2 if current stmt is the first
        live_in:        set of variables alive when entering the current stmt node
        live_out:       set of variables alive when exiting the current stmt node 
        def:            set of variables defined within the current stmt node
        use:            set of variables used within the currrent stmt node *)
type stmt_node =
    {
      id_stmt: int;
      stmt: ir3_stmt;
      succ: int;
      pred: int;
      live_in: id3_set;
      live_out: id3_set;
      def: id3_set;
      use: id3_set;
    }

(* hash table for the statements *)
type stmtsHT = (int, stmt_node) Hashtbl.t

(* variables explanation for the basic block :
        id_babl:        unique integer defining a basic block
        entry:          indicates whether the current block is the entring block
        exit:           indicates whether the current block is the ending block
        stmts:          hashtable of stmts within the babl with info on liveness
        succ:           list of id_babl in input of the current basic block
        pred:           list of id_babl in output of the current basic block
        live_in:        set of variables alive when entering the current basic block
        live_out:       set of variables alive when exiting the current basic block 
        def:            set of variables defined within the current basic block
        use:            set of variables used within the currrent basic block *)
type basic_block =
    {
      id_babl: string;
      entry: bool;
      exit: bool;
      stmts: stmtsHT;
      succ: string list;
      pred: string list;
      live_in: id3_set;
      live_out: id3_set;
      def: id3_set;
      use: id3_set;
    }

(* hash table of the basic blocks *)
type blocks = (int, basic_block) Hashtbl.t

(* define methods hash table for further use. string is the method's name and
   md_babl is the basic blocks type *)
type mdHT = (string, blocks) Hashtbl.t

(* methods to implement:
        get a specific stmt_node
        get a block
        get the first/last node *)

(* prints a set s *)
  let print_set s = 
     Id3_set.iter print_endline s;;

let babl_label = ref 0
(* returns a fresh label for the id_babl *)
let fresh_babl_label (md:md_decl3) : string =
    let name = md.id3 in
    (babl_label:=!babl_label+1; name^"_"^(string_of_int !babl_label))

let stmt_label = ref 0
(* returns a fresh label for the id_stmt *)
let fresh_stmt_label () : int = 
    (stmt_label:=!stmt_label+1; !stmt_label)
 
(* generate the basic blocks of a method *)
let ir3_program_to_basic_blocks
    (mds:md_decl3 list) (ir3prog:ir3_program) =
    let _ = print_endline("Printing the Id3_set ...") in
    let s = Id3_set.singleton "\tfoo" in
    print_set s



