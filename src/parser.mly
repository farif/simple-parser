%{ open Ast %}

%token <int> Int
%token <string> Id

(* node *)
%token STATE INIT NEXT

(* I/O *)
%token INPUT OUTPUT

(* properties under consideration *)
%token CONSTRAINT BAD

(* Type definitions *)
%token SORT BITVEC ARRAY

(* constants *)
%token ZERO ONE ONES CONST CONSTD CONSTH

(* array operations *)
%token WRITE READ

(* arithmetic operations *)
%token ADD MUL SUB UDIV UREM
%token SDIV SMOD SREM

(* Unsigned/Signed inequality *) 
%token UGT UGTE ULT ULTE 
%token SGT SGTE SLT SLTE

(* Boolean *)
%token NOT AND NAND NOR OR XNOR XOR

%token NEQ EQ IMPLIES IFF 
%token ITE

%token NEG
%token REDAND REDOR REDXOR  

%token ROL ROR SLL SRA SRL

%token SLICE UEXT SEXT CONCAT

(* Overflow *)
%token UADDO SADDO UDIVO SDIVO UMULO SMULO USUBO SSUBO

%token COMMENT
%token EOF


%start main
%type <Ast.btor> main

%%

main: bp = btor EOF { Nodes bp }

btor: 
    |  n = node { [n] }
    |  n = node m = btor {n :: m}

sort:
    | BITVEC num = Int { BitVec(num) }
    | ARRAY sid1 = Int sid2 = Int { Array(BitVec(sid1), BitVec(sid2))}

node:
    | sid = Int SORT s = sort { Sort(sid, s) }
    | nid = Int STATE sid = Int str = Id { State(nid, BitVec(sid), str) }
    | nid = Int INPUT sid = Int  str = Id {Input(nid, BitVec(sid), str)}
    | nid = Int v = value sid = Int {Value(nid, BitVec(sid), v)}
    | nid = Int CONST sid = Int v = Int {Value(nid, BitVec(sid), Const(v))}

value:
    | ONE   {One}
    | ONES  {Ones}
    | ZERO  {Zero}

operator:
    | AND  {AND}