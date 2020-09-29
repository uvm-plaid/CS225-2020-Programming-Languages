(* CS225 RPN Calculator implementation *)

(* 
   calculator type definition
   implementation provides various functions for an RPN calculator
*)
type calculator = { 
    add : unit -> int; 
    subtr : unit -> int; 
    enter : int -> int;
    store : unit -> int;
    recall : unit -> int;
    clear : unit -> int;
    clear_all : unit -> int
  }

(* an implementation of the calculator type *)
let calc = 

  (* mutable list of operands *)
  let operand_stack = ref [] in

  (* mutable stored value *)
  let stored = ref 0 in {           

  add = 
  (* 
     in : ()
     out : if operand_stack stores [x,y,...] then x + y
           if operand_stack stores [x] then x
           if operand_stack stores [] then 0
     mutates : operand_stack if operand_stack stores [x,y,...],
               so that x and y are replaced by x + y
   *)       
  (fun () -> 
    match !operand_stack with
      [] -> 0
    | [x] -> x
    | x::y::xs -> operand_stack := (x+y)::xs; x+y);
  
  subtr = 
  (* 
     in : ()
     out : if operand_stack stores [x,y,...] then y - x
           if operand_stack stores [x] then -x
           if operand_stack stores [] then 0
     mutates : operand_stack if operand_stack stores [x,y,...],
               so that x and y are replaced by y - x
  *) 
  (fun () -> 
    match !operand_stack with
      [] -> 0
    | [x] -> -x
    | x::y::xs -> operand_stack := (y - x)::xs; y - x);

  enter = 
  (* 
     in : integer x
     out : x
     mutates : operand_stack, adds x to top of stack
  *)
  (fun x -> operand_stack := x :: !operand_stack; x);

  store = 
  (* 
     in : ()
     out : top of operand_stack or 0 if operand_stack = []
     mutates : stored, replaces contents with value at top of 
               operand_stack, zero if latter is empty
  *)
  (fun _ -> 
    match !operand_stack with 
      [] -> stored := 0; 0
    | x::xs -> stored := x; x);
  
  recall = 
  (*
     in : ()
     out : !stored
     mutates : operand_stack, !stored placed at top
  *)
  (fun _ -> operand_stack := !stored :: !operand_stack; !stored);

  clear = 
  (*
     in : () 
     out : 0
     mutates : operand_stack, resets it to []
  *)
  (fun _ -> operand_stack := []; 0);

  clear_all = 
  (*
     in : () 
     out : 0
     mutates : operand_stack and stored, resets to [] and 0 
               respectively
  *)
  (fun _ -> operand_stack := []; stored := 0; 0)
} 
    
