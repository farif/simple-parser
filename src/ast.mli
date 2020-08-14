(* Sort Type:
    a.) BitVector
    b.) Arrays of type BitVvector  *)
  type sort =
    | Sid of int
    | BitVec of  int       (* size*)
    | Array of   sort * sort   

  type value = 
    | One                 (* Input Values range over One, Zero, Zeros*)
    | Ones
    | Zero
    | Const  of int       (* BitVector Constants in Bits, lateron update to Bool *)
    | Constd of int       (* BitVector Constants in Decimal *)
    | Consth of int       (* BitVector Constants in Hex, Later on update to Hex *)
 
(* Indexed Operator *) 
  type opidx = 
    | SLICE
    | SEXT
    | UEXT


(* Unindexed Operator *)
  type op = 
    | AND
    | OR
    | XOR
    | IFF 
    | ADD
    | EQ

(* Property Type *)
  type ptype = 
    | BAD 
    | CONSTRAINT
    | FAIR
    | OUTPUT
    | JUSTICE 

(* node can be a sort, nid, nodetype, safety-property, or an operation *)
  type node =    
    | Nid of int      (* Node Identifier *)
    | Sort of  int * sort       (* Sort is of BitVec or Array over BitVec *) 
    | Input of int * sort * string     (* Node Type is of type Input or State *) 
    | State of int * sort * string     (* State is of type Init or Next *)    
    | Init of  int * sort * node * node 
    | Next of  int * sort * node * node 
    | Value of int * sort * value (* Constant Values Range over Types *) 
    | Op of int * sort * node * node
    | OpIdx of int * opidx * node * node 
    | Prop of int * ptype * node

(* BTOR2 program is a list of nodes. *) 
  type btor = 
    | Nodes of node list
    | EOF 

