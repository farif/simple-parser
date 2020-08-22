(* Sort Type:
    a.) BitVector
    b.) Arrays of type BitVvector  *)
  type sort =
    | BitVec of  int       (* size *)
    | Array of   sort * sort   

  type input = 
    | InputV of sort * string

  type state = 
    | StateV of sort * string


  type value = 
    | One   of sort              (* Input Values range over One, Zero, Zeros*)
    | Ones  of sort
    | Zero  of sort
    | Const  of sort * int       (* BInput(sid, i); itVector Constants in Bits, lateron update to Bool *)
    | Constd of sort * int       (* BitVector Constants in Decimal *)
    | Consth of sort * int       (* BitVector Constants in Hex, Later on update to Hex *)
 
(* Indexed Operator *) 
  type opidx = 
    | Slice
    | Sext
    | Uext

(* Unindexed Operator *)
  type uop = 
    | Not 
    | Neg  
    | Redand
    | Redor
    | Redxor

  type bop =     
    (* Boolean *)
    | And
    | Nand
    | Nor 
    | Or
    | Xor
    | Xnor   
    | Implies 
    | Iff
        

    | Eq
    | Neq

    | Ugt
    | Sgt
    | Ugte
    | Sgte
    | Ult
    | Slt
    | Ulte
    | Slte

    (* Rotate, Shift *)

    | Rol
    | Ror
    | Sll
    | Srl
    | Sra

    (* Arithmetic *)
    | Add
    | Sub
    | Mul

    | Udiv
    | Sdiv
    | Smod
    
    | Urem
    | Srem

    (* Overflow *)
    | Uaddo
    | Saddo
    | Udivo
    | Sdivo
    | Umulo
    | Smulo
    | Usubo
    | Subo

    | Concat
    
    | Read

type cond_op = 
    | Ite 
    | Write

(* Property Type *)
  type ptype = 
    | Bad 
    | Constraint
    | Output
    | Fair
    | Justice 

(* node can be a sort, nid, nodetype, safety-property, or an operation *)
  type node =    
    | Sort of  int * sort       (* Sort is of BitVec or Array over BitVec *) 
    | Input of int * input     (* Node Type is of type Input or State *) 
    | State of int * state     (* State is of type Init or Next *)    
    | Init of  int * sort * node * node 
    | Next of  int * sort * node * node 
    | Value of int * value (* Constant Values Range over Types *) 
    | Uop of int * sort * uop * node
    | Bop of int * sort * bop * node * node
    | OpIdx of int * sort * opidx * node * int * int   (* Slice *)
    | Cond of int * sort * cond_op * node * node * node
    | Prop of int * ptype * node

(* BTOR2 program is a list of nodes. *) 
  type btor = 
    | Nodes of node list
    | EOF 
