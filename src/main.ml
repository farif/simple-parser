(* 
    let () = Printf.printf ">>" 
    let p = read_line()
*) 

let p1 = "
        1 sort bitvec 3
        2 zero 1
        3 state 1 A
        4 init 1 3 2
        5 one 1
        6 add 1 3 5
        7 next 1 3 6
        8 ones 1
        9 sort bitvec 1
        10 eq 9 3 8
        11 bad 10         
        "

let p2 = "
        1 sort bitvec 1
        2 sort bitvec 3
        3 input 1 TURN
        4 zero 2
        5 state 2 A
        6 init 2 5 4
        7 constd 2 2
        8 add 2 5 7
        9 ite 2 3 5 8
        10 next 2 5 9
        11 one 2
        12 eq 1 5 11
        13 bad 12
        "

let p3 = "
        1 sort bitvec 4
        2 one 1
        3 state 1 FACTORIAL
        4 state 1 I
        5 init 1 3 2
        6 init 1 4 2
        7 add 1 4 2
        8 mul 1 3 4
        9 next 1 4 7
        10 next 1 3 8
        11 ones 1
        12 sort bitvec 1
        13 eq 12 4 11
        14 bad 13
        15 slice 12 3 0 0
        16 constd 1 3
        17 ugt 12 4 16
        18 and 12 17 15
        19 bad 18
        "

let p4 = " 1 sort bitvec 1
        2 sort bitvec 32
        3 input 1 TURN
        4 zero 2
        5 state 2 A
        6 state 2 B
        7 init 2 5 4
        8 init 2 6 4
        9 one 2
        10 add 2 5 9
        11 add 2 6 9
        12 ite 2 3 5 10
        13 ite 2 3 6 11
        14 next 2 5 12
        15 next 2 6 13
        16 constd 2 3
        17 eq 1 5 16
        18 eq 1 6 16
        19 and 1 17 18
        20 bad 19
    "

let n = Parser.main Lexer.read (Lexing.from_string p2) 

let () =  print_endline  ("Parsed Program (AST):") ; Util.print_btor n  
 