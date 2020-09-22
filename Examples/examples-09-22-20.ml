let (x,y) = (1,2) in x + y;;

match (1,2,3) with 
    (0,x,y) -> x + y 
  | (1,x,y) -> x * y;;
  
let rec fact n = 
   match n with 
      0 -> 1
    | _ -> n * (fact (n-1));;
 
let thrd (_,_,x) = x;;

(thrd : int * int * int -> int);;

(thrd : int * int * string -> string);;

(thrd : int * int * int -> string);;  (* nope! *)

let compose f g = (fun x -> f(g(x)));;



    
