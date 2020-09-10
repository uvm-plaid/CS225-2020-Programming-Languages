(* CS225 Examples 9/10/20 *)

(fun y -> 3 + y);;

(fun y -> 3 + y) 2;;

(fun y -> if y then 1 else 2) false;;

(fun y -> if y then 1 else 2) 5;;  (* type error, semantic error *)

(fun y -> if y then true else 2) false;;  (* type error *)

let f = (fun x -> x + x) in f 4;;    (* let-binding a function value *)

let plus = (fun x -> fun y -> x + y);;  (* currying *)

plus 3 8;;

let add1 = plus 1 in add1 4;;

let rec expt = (fun x -> if x = 0 then 1 else 2 * expt(x - 1);; (* recursion! *)

let rec fact = (fun x -> if x = 0 then 1 else x * fact(x - 1));; 

let plus x y = x + y;;   (* sugared form *)

let fact n = if n = 0 then 1 else n * (fact (n-1));;    (* sugared form *)

let pi = 3.14;;
let area r = 2.0 *. pi *. r *. r;;   (* area is a closure, references non-local pi *)

let pi = 12345.5 in area 2.0;;    (* illustrates closure semantics *)
