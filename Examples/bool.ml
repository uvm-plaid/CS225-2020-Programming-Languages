type boolexp = Bool of bool | And of boolexp * boolexp | Or of boolexp * boolexp | Not of boolexp;;

let rec redx exp = match exp with
     Not(Bool(false)) -> Bool(true) 
   | Not(Bool(true)) -> Bool(false)
   | And(Bool(_), Bool(false)) -> Bool(false)
   | And(Bool(true), Bool(true)) -> Bool(true)
   | And(Bool(false), Bool(_)) -> Bool(false)
   | Or(Bool(true), Bool(_)) -> Bool(true)
   | Or(Bool(false), Bool(false)) -> Bool(false)
   | Or(Bool(false), Bool(true)) -> Bool(true)
   | Not(e) -> redx e
   | And(Bool(b),e) -> And(Bool(b), redx e)
   | And(e1,e2) -> And(redx e1, e2)
   | Or(Bool(b),e) -> And(Bool(b), redx e)
   | Or(e1,e2) -> And(redx e1, e2);;
   
let rec redxs exp = match exp with 
      Bool(b) -> Bool(b)
    | _ -> redxs (redx exp);;
    
let e1 = And(Bool(true), Bool(false));;

let e2 = Not(e1);;

let e3 = Or(e1, e2);;
