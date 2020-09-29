type student = { name : string; email : string; gpa : float};;

let bob = { name = "bob"; email = "bob@uvm.edu"; gpa = 3.2};;
    
type rt = {a: int; b: bool};;

let rv = { a = 1 + 2; b = true};;

let x = ref 0 in (x := 1; x := 2; !x + 2);;

let x = ref 0 in (x := 1; 1) + (x := 2; 1) + !x;;

let rec fibnum n = match n with
    0 -> 0
  | 1 -> 1 
  | _ -> fibnum (n-1) + fibnum (n-2);;
 
let rec fibnum_mem n =
  let tab = Array.make (n+1) 0 in 
  let rec fm n = match n with
    0 -> 0
  | 1 -> 1 
  | n -> if tab.(n) > 0 then tab.(n) else let fn = fm (n-1) + fm (n-2) in (tab.(n) <- fn; fn)
in fm n;;

exception Negative;;

let rec fact n = 
  if n < 0 then raise Negative else 
  match n with
     0 -> 1
   | n -> n * fact (n-1);;
   
type 'a option = None | Some of 'a;;

let fact_option n = try Some(fact n) with Negative -> None;;
