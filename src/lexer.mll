{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let white = ['\t' ' ']
let newline = '\r' | '\n' | "\r\n"

let digits = ['0' - '9']
let alpha = ['A'-'Z' '_']

(* BTOR2 Lexer *)

rule read = parse    
    | white    { read lexbuf }
    | newline  { next_line lexbuf; read lexbuf }    
    | eof { EOF }

    | (digits)+ as d  { Int (int_of_string d) }
    | (alpha)+ as s {Id s}

    | "sort" {SORT}        (* Sort Type *) 
    | "bitvec" {BITVEC}    (* BitVector / Register *)
    | "array" {ARRAY}      (* Array / Memory *)

    | "state" {STATE}
    | "input" {INPUT}    
    | "init" {INIT}        (* Initialization *)
    | "next" {NEXT}        (* Successor *)

    | "output" {OUTPUT}

    (* Properties *)
    | "bad" {BAD}
    | "constraint" {CONSTRAINT}

    | "const" {CONST}        (* Bitvector Constant *)
    | "constd" {CONSTD}      (* Bitvector Constant Decimal *)   
    | "consth" {CONSTH}      (* Bitvector Constant Hex *) 

    | "one" {ONE}       (* ? *)
    | "ones" {ONES}     (* Bitvector bv1 *)
    | "zero" {ZERO}     (* Bitvector bv0 *)

    (* ------------------------- *)
    (* [opidx] Indexed operators *)
    (* ------------------------- *)
    | "uext" {UEXT}      (* Unsigned extension *)
    | "sext" {SEXT}      (* signed extension *)    
    | "slice" {SLICE}   (* extraction *)   

    (* -------------------------------- *)
    (* [op] Unary unindexed operators  *)
    (* -------------------------------- *)
    | "not" {NOT}       (* Bit-wise *)
    | "neg" {NEG}       (* arithmetic *)

    | "redand" {REDAND}  (* reduction *)
    | "redor" {REDOR}    (* reduction *)
    | "redxor" {REDXOR}  (* reduction *)

    (* -------------------------------- *)
    (* Binary unindexed operators / op  *)
    (* -------------------------------- *)
    | "iff" {IFF}           (* Boolean *)
    | "implies" {IMPLIES}   (* Boolean *)

    | "eq" {EQ}             (* Equality *)
    | "neq" {NEQ}           (* Dis-equality *)

    (* Unsigned/Signed inequality *)
    | "ugt" {UGT}
    | "sgt" {SGT}
    | "ugte" {UGTE}
    | "sgte" {SGTE}
    | "ult" {ULT}
    | "slt" {SLT}
    | "ulte" {ULTE}
    | "slte" {SLTE}

    (* Boolean *)
    | "and" {AND}
    | "nand" {NAND}
    | "nor" {NOR}
    | "or" {OR} 
    | "xnor" {XNOR}
    | "xor" {XOR}

    (* Rotate, Shift *)
    | "rol" {ROL}
    | "ror" {ROR}
    | "sll" {SLL}
    | "sra" {SRA}
    | "srl" {SRL}

    (* Arithmetic *)
    | "add" {ADD}
    | "mul" {MUL}
    | "udiv" {UDIV}
    | "sdiv" {SDIV}
    | "smod" {SMOD}
    | "urem" {UREM}
    | "srem" {SREM}
    | "sub" {SUB}

    (* Overflow *)
    | "uaddo" {UADDO}
    | "saddo" {SADDO}
    | "udivo" {UDIVO}
    | "sdivo" {SDIVO}
    | "umulo" {UMULO}
    | "smulo" {SMULO}
    | "usubo" {USUBO}
    | "ssubo" {SSUBO}

    (* Concatenation *)
    |"concat" {CONCAT}

    (* Array Read *)
    | "read" {READ}

    (* -------------------------- *)
    (* (opidx) Ternary Operators *)
    (* -------------------------- *)
    | "ite" {ITE}      (* Conditional *)
    | "write" {WRITE}  (* Array Write *)

    (* Utility Function *)
    | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
 
