%{ 
    open Ast 

    let sort_map = Hashtbl.create 100
    let node_map = Hashtbl.create 100

    let get_nid = function
       | Sort(nid, _) -> nid
       | State(nid, _) -> nid
       | Input(nid,_) -> nid
       | Init(nid,_,_,_) -> nid
       | Next(nid,_,_,_) -> nid
       | Value(nid, _) -> nid
       | Uop(nid,_,_,_) -> nid
       | Bop(nid,_,_,_,_) -> nid
       | OpIdx(nid,_,_,_,_,_) -> nid
       | Prop(nid,_,_) -> nid
       | Cond(nid,_,_,_,_,_) -> nid

%}

%token <int> Int
%token <string> Id

(* node *)
%token STATE INIT NEXT

(* I/O *)
%token INPUT OUTPUT

(* properties under consideration *)
%token CONSTRAINT BAD FAIR JUSTICE

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
    |  n = node m = btor { n :: m }

sort:
    | SORT BITVEC num = Int { BitVec(num) }
    | SORT ARRAY sid1 = Int sid2 = Int { Array(Hashtbl.find sort_map sid1, Hashtbl.find sort_map sid2) } 

input:
    | INPUT sid = Int  str = Id { InputV(Hashtbl.find sort_map sid, str) }
    | INPUT sid = Int  { InputV(Hashtbl.find sort_map sid, "") }

state:
    | STATE sid = Int  str = Id { StateV(Hashtbl.find sort_map sid, str) }
    | STATE sid = Int  { StateV(Hashtbl.find sort_map sid, "") }

value:
    | ONE sid = Int {One(Hashtbl.find sort_map sid)}
    | ONES sid = Int {Ones(Hashtbl.find sort_map sid)}
    | ZERO sid = Int {Zero(Hashtbl.find sort_map sid)}
    | CONST  sid = Int v = Int { Const(Hashtbl.find sort_map sid, v) } 
    | CONSTD sid = Int v = Int { Constd(Hashtbl.find sort_map sid, v) }
    | CONSTH sid = Int v = Int { Consth(Hashtbl.find sort_map sid, v) }

cond_op:
    | ITE {Ite}
    | WRITE {Write}

opidx:
    | SLICE {Slice} 
    | SEXT {Sext}
    | UEXT {Uext}

property:
    | BAD  {Bad}
    | CONSTRAINT {Constraint}
    | FAIR  {Fair}
    | OUTPUT {Output}
    | JUSTICE {Justice}

node:
    | sid = Int s = sort  { Hashtbl.add sort_map sid s; Sort(sid, s) } 
    | n = node_exp  { Hashtbl.add node_map (get_nid n) n; n }

node_exp:
    | nid = Int i = input  { Input(nid, i) }
    | nid = Int st = state { State(nid, st) }
    | nid = Int INIT  sid = Int nid1 = Int nid2 = Int { Init(nid, Hashtbl.find sort_map sid, Hashtbl.find node_map nid1, Hashtbl.find node_map nid2) } 
    | nid = Int NEXT  sid = Int nid1 = Int nid2 = Int { Next(nid, Hashtbl.find sort_map sid, Hashtbl.find node_map nid1, Hashtbl.find node_map nid2) }
    | nid = Int v = value { Value(nid, v) }
    | nid = Int op = uop sid = Int nid1 = Int { Uop(nid, Hashtbl.find sort_map sid, op, Hashtbl.find node_map nid1) }
    | nid = Int op = bop sid = Int nid1 = Int nid2 = Int { Bop(nid, Hashtbl.find sort_map sid, op, Hashtbl.find node_map nid1, Hashtbl.find node_map nid2) }
    | nid = Int op = opidx sid = Int nid1 = Int uint1 = Int uint2 = Int { OpIdx(nid, Hashtbl.find sort_map sid, op, Hashtbl.find node_map nid1, uint1, uint2) }

    | nid = Int op = cond_op sid = Int nid1 = Int nid2 = Int nid3 = Int { Cond(nid, Hashtbl.find sort_map sid, op, Hashtbl.find node_map nid1, Hashtbl.find node_map nid2, Hashtbl.find node_map nid3) }    

    | nid = Int prop = property nid1 = Int { Prop(nid, prop, Hashtbl.find node_map nid1) }

uop:
    | NOT {Not}
    | NEG {Neg}

    | REDAND {Redand}
    | REDOR {Redor}
    | REDXOR {Redxor}

bop:
    (* Boolean *)
    | AND {And}
    | NAND {Nand}
    | NOR {Nor}
    | OR {Or}
    | XOR {Xor}
    | XNOR {Xnor}   
    | IMPLIES {Implies} 
    | IFF {Iff}
        

    | EQ  {Eq}
    | NEQ {Neq}

    | UGT {Ugt}
    | SGT {Sgt}
    | UGTE {Ugte}
    | SGTE {Sgte}
    | ULT {Ult}
    | SLT {Slt}
    | ULTE {Ulte}
    | SLTE {Slte}

    (* Rotate, Shift *)

    | ROL {Rol}
    | ROR {Ror}
    | SLL {Sll}
    | SRA {Sra}
    | SRL {Srl}

    (* Arithmetic *)
    | ADD {Add}
    | SUB {Sub}
    | MUL {Mul}

    | UDIV {Udiv}
    | SDIV {Sdiv}
    | SMOD {Smod}
    
    | UREM {Urem}
    | SREM {Srem}

    (* Overflow *)
    | UADDO {Uaddo}
    | SADDO {Saddo}
    | UDIVO {Udivo}
    | SDIVO {Sdivo}
    | UMULO {Umulo}
    | SMULO {Smulo}
    | USUBO {Usubo}
    | SSUBO {Subo}

    | CONCAT {Concat}
    
    | READ {Read}

