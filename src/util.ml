open Ast

let rec print_sort = function
    | Sid n  -> string_of_int n
    | BitVec size -> "BV("^ string_of_int size ^")" 
    | Array (s1,s2) -> "Array("^ (print_sort s1) ^ "," ^ (print_sort s2) ^")"


let print_value = function
    | One   -> "one"
    | Ones  -> "ones"
    | Zero  -> "zero"
    | Const v -> "const("^ string_of_int v ^")"
    | _ -> "Not supported: value"

let print_node = function
    | Nid n -> string_of_int n
    | Sort (sid, s) -> "sort(" ^ string_of_int sid ^ ","  ^ print_sort s ^")" 
    | State (nid, s, str) -> "state(" ^ string_of_int nid ^ "," ^ (print_sort s) ^ "," ^ str ^")"
    | Input (nid, s, str) -> "Input(" ^ string_of_int nid ^ "," ^ (print_sort s) ^ "," ^ str ^")"
    | Value (nid, s, v) -> "Value(" ^ string_of_int nid ^ "," ^ (print_sort s) ^ "," ^ (print_value v) ^ ")"
    | _ -> "Not supported: node"

let rec print_btor = function
    | EOF -> "Empty"
    | Nodes nodes -> match nodes with
                      | [] -> ""
                      | h :: t  -> print_node h ^"\n" ^ print_btor (Nodes t) 




