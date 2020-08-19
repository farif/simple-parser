open Ast

let rec print_sort = function
    | BitVec size -> "BV("^ string_of_int size ^")" 
    | Array (s1,s2) -> "Array("^ (print_sort s1) ^ "," ^ (print_sort s2) ^")"

let print_input = function
    | InputV (sort, label) -> "InputV("^  label ^ ":" ^ print_sort sort  ^")" 

let print_state = function
    | StateV (sort, label) -> "StateV("^ label ^ ":" ^ print_sort sort  ^")" 

let print_value = function
    | One  sort -> "one" ^ ":" ^ print_sort sort  ^")" 
    | Ones sort -> "ones" ^ ":" ^ print_sort sort  ^")" 
    | Zero sort -> "zero" ^ ":" ^ print_sort sort  ^")" 
    | Const (sort, v) -> "const("^ string_of_int v ^ ":" ^ print_sort sort ^")"
    | Constd (sort, v) -> "constd("^ string_of_int v ^  ":" ^ print_sort sort ^")"
    | Consth (sort, v) -> "consth("^ string_of_int v ^  ":" ^ print_sort sort ^")"

let print_bop = function
    (* Boolean *)
    | And -> "And"
    | Nand -> "Nand"
    | Nor -> "Nor"
    | Or -> "Or"
    | Xor -> "Xor"
    | Xnor -> "Xnor"  
    | Implies -> "Implies"
    | Iff -> "Iff"
        
    | Eq -> "Eq"
    | Neq -> "Neq"

    | Ugt -> "Ugt"
    | Sgt -> "Sgt"
    | Ugte -> "Ugt"
    | Sgte -> "Sgt"
    | Ult -> "Ult"
    | Slt -> "Slt"
    | Ulte -> "Ulte"
    | Slte -> "Slte"

    (* Rotate, Shift *)

    | Rol -> "Rol"
    | Ror -> "Ror"
    | Sll -> "Sll"
    | Srl -> "Srl"
    | Sra -> "Sra"

    (* Arithmetic *)
    | Add -> "Add"
    | Sub -> "Sub"
    | Mul -> "Mul"

    | Udiv -> "Udiv"
    | Sdiv -> "Sdiv"
    | Smod -> "Smod"
    
    | Urem -> "Urem"
    | Srem -> "SRem"

    (* Overflow *)
    | Uaddo -> "Uaddo"
    | Saddo -> "Saddo"
    | Udivo -> "Udivo"
    | Sdivo -> "Sdivo"
    | Umulo -> "Umulo"
    | Smulo -> "Smulo"
    | Usubo -> "Usubo"
    | Subo -> "Subo"

    | Concat -> "Concat"
    
    | Read -> "Read"


let print_uop = function
    | Not -> "Not" 
    | Neg  -> "Neg"
    | Redand -> "Redand"
    | Redor -> "Redor"
    | Redxor -> "Redxor"
    
let print_idx = function
    | Slice -> "Slice"
    | Sext -> "Sext"
    | Uext -> "Uext"

let print_cond = function
    | Ite -> "Ite"
    | Write -> "Write"

let print_pp = function
    | Bad -> "Bad"
    | Constraint -> "Constraint"
    | Output -> "Output"
    | Fair -> "Fair"
    | Justice -> "Justice"

let rec print_node = function
    | Sort (sid, s) -> "sid:"^ string_of_int sid  ^ " sort(" ^ print_sort s ^")" 
    
    | State (nid, s) -> "nid:"^ string_of_int nid ^ " state(" ^ (print_state s) ^")"
    | Input (nid, s) -> "nid:"^ string_of_int nid ^"  input(" ^ (print_input s) ^")"   
    | Value (nid, v) -> "nid:"^ string_of_int nid ^ " value(" ^(print_value v) ^")"

    | Init (nid, s, n1, n2) -> "nid:"^ string_of_int nid ^ " init(" ^ (print_sort s) ^ "," ^ (print_node n1) ^ "," ^ (print_node n2) ^")"
    | Next (nid, s, n1, n2) -> "nid:"^ string_of_int nid ^ " next(" ^ (print_sort s) ^ "," ^ (print_node n1) ^ "," ^ (print_node n2) ^")"

    | Uop (nid, s, op, n) -> "nid:"^ string_of_int nid ^ " " ^ (print_uop op) ^ "("^ (print_node n) ^ ") :" ^ (print_sort s)
    | Bop (nid, s, op, n1, n2) -> "nid:"^ string_of_int nid ^ " " ^ (print_bop op) ^ "("^ (print_node n1) ^ ","^ (print_node n2) ^ ") :" ^ (print_sort s)
    | OpIdx (nid, s, op, n, u1, u2) -> "nid:"^ string_of_int nid ^ " " ^ (print_idx op) ^ "("^ (print_node n) ^ (string_of_int u1) ^ ","^ (string_of_int u2) ^ ") :" ^ (print_sort s)
    | Cond (nid, s, op, n1, n2, n3) -> "nid:"^ string_of_int nid ^ " " ^ (print_cond op) ^ "("^ (print_node n1) ^ ","^ (print_node n2) ^ (print_node n3) ^ ") :" ^ (print_sort s)
    | Prop (nid, op, n) -> "nid:"^ string_of_int nid ^ " " ^ (print_pp op) ^ "[" ^ (print_node n) ^"]"
    
    
let pp_node n = print_endline (print_node n)

let print_btor = function
    | EOF -> ()
    | Nodes nodes ->  List.iter pp_node nodes