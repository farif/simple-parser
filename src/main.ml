(* 
    let () = Printf.printf ">>" 
    let p = read_line()
*) 

let p = "1 sort bitvec 10
         2 sort bitvec 5
         3 sort array 1 2
         4 input 1 TURN
         5 state 2 B
         6 one 2    
         7 const 2 10
        "

let n = Parser.main Lexer.read (Lexing.from_string p) 

let () =  print_endline  ("Parsed Program (AST):\n" ^ (Util.print_btor n))  
 