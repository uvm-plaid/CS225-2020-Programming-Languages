(*
   Graduate Student Programming Assignment 3: Completing the CatML_forall type checker.

   In this OCaml code file you will find the beginning of various functions with
   COMPLETE ME tags in the comments, that you must complete to obtain a *correct*
   type checker for the CatML_forall language.

   A top-level typing function with pretty printing (pptyping) has been completed
   for you and can be used for testing examples as you work on the assignment. The
   online tool at goatgoose can be used to easily generate ASTs and test examples.

   You should submit this file once completed. Your submission must be executable
   OCaml code.
*)

(*
   Abstract Syntax
   ---------------

   The tau datatype defines the ASTs for CatML_forall types. The mapping from concrete
   type syntax to abstract type syntax is defined as follows. This mapping is implemented
   by the parser in the online tool at goatgoose.

   [[Nat]] = Natt
   [[Bool]] = Boolt
   [[tau_1 * tau_2]] = Prod([tau_1]], [[tau_2]])
   [[tau_1 -> tau_2]] = Arrow([[tau_1]], [[tau_2]])
   [['a]] = TVar(Ident("'a"))
   [[V 'a . tau]] = Forall(Ident("'a"), [[tau]])
   
   The expr datatype defines the ASTs for CatML_forall expressions. The mapping from concrete syntax
   to abstract syntax is as follows, in full detail. This mapping is implemented by the
   parser in the online tool at goatgoose.
 
   [[True]] = Bool(true)
   [[False]] = Bool(false)
   [[n]] = Nat(n)           for any natural number n
   [[x]] = Var(Ident("x"))       for any variable x
   [[e1 + e2]] = Plus([[e1]], [[e2]])
   [[e1 - e2]] = Minus([[e1]], [[e2]])
   [[e1 And e2]] = And([[e1]], [[e2]])
   [[e1 Or e2]] = Or([[e1]], [[e2]])
   [[Not e]] = Not([[e]])
   [[(e1, e2)]] = Pair([[e1]], [[e2]])
   [[Fst(e)]] = Fst([[e]])
   [[Snd(e)]] = Snd([[e]])
   [[e1 e2]] = Appl([[e1]], [[e2]])
   [[Let x = e1 in e2]] = Let(Ident("x"), [[e1]], [[e2]])
   [[(Fun (x : tau) . e)]] = Fun(Ident("x"), [[tau]], [[e]])
   [[(Fix z . (x : tau_1) : tau_2 . e)]] = Fix(Ident("z"), Ident("x"), [[tau_1]], [[tau_2]], [[e]])
   [[(Gen 'a . e)]] = Gen(Ident("'a"), [[e]])
   [[e<tau>]] = Inst([[e]], [[tau]])
*)

type ident = Ident of string

(* type syntax *)
type tau = Natt | Boolt | Prod of tau * tau | Arrow of tau * tau |
           TVar of ident | Forall of ident * tau 

(* expression syntax *)
type expr =
     (* boolean expression forms *)
     Bool of bool | And of expr * expr | Or of expr * expr | Not of expr   
     (* arithmetic expression forms *)
   | Nat of int | Plus of expr * expr | Minus of expr * expr | Equal of expr * expr  
     (* functional expression forms *)
   | Function of ident * tau * expr | Appl of expr * expr | Var of ident
     (* pairs *)
   | Pair of expr * expr | Fst of expr | Snd of expr
     (* control forms *)
   | If of expr * expr * expr | 
     Let of ident * expr * expr |
     Fix of ident * ident * tau * tau * expr 
     (* type generalization and instantiation *)
   | Gen of ident * expr | Inst of expr * tau

(* this exception should get raised when a type error is encountered *)
exception TypeError

exception AssignmentIncomplete

(* type environments datatype *)
type environment = (ident * tau) list

(* 
   lookup : ident -> environment -> tau
   in : identifier x, environment gamma
   out : gamma(x)
*)
let rec lookup x gamma = 
  match gamma with 
    [] -> raise TypeError
  | (x',t)::bs -> if x = x' then t else lookup x bs

(* 
   extend : environment -> ident * tau -> environment
   in : environment gamma, identifier, type pair (x,tau)
   out : (gamma; x : tau)
*)
let rec extend gamma (x,y) = (x,y)::gamma

(* 
   typecheck : environment -> expr -> tau
   in : type environment gamma, TD expression e
   out : tau such that (gamma |- e : tau) is a valid type judgement
   side effect: raises TypeError if e is not well-typed
*)
let rec typecheck gamma e = 
  match e with
  | Nat(x) -> Natt
  | Plus(x, y) -> 
      (match (typecheck gamma x, typecheck gamma y) with 
	(Natt, Natt) -> Natt
      |	_ -> raise TypeError)      
  | _ -> raise AssignmentIncomplete

(* 
   typing : expr -> tau
   in : expression e
   out : tau such that e : tau
   side effect: raises TypeError if e is not well-typed
   note : this is the top-level function used by the interpreter, and is 
          defined in terms of typecheck; there is no need to alter this 
          function in any way
*)
let typing e = typecheck [] e

(*
  pptau : tau -> string
  in : A type AST [[tau]].
  out : The concrete expression tau in string format.
*)
let rec pptau = function
    Natt -> "Nat"
  | Boolt -> "Bool"
  | Prod(t1,t2) -> "(" ^ pptau t1 ^ " * " ^ pptau t2 ^ ")"
  | Arrow(t1,t2) -> "(" ^ pptau t1 ^ " -> " ^ pptau t2 ^ ")"
  | Forall(Ident(x), t) -> "V " ^ x ^ " . " ^ pptau t
  | TVar(Ident(x)) -> x

(*
  ppexpr : expr -> string
  in : An expression AST [[e]].
  out : The concrete expression e in string format.
*)
let rec ppexpr e = match e with
   | Bool true -> "True"
   | Bool false -> "False"
   | Nat n -> string_of_int n
   | Var(Ident(x)) -> x
   | And (e1, e2) -> "(" ^ (ppexpr e1) ^ " And " ^ (ppexpr e2) ^ ")"
   | Or (e1, e2) -> "(" ^ (ppexpr e1) ^ " Or " ^ (ppexpr e2) ^ ")"
   | Not e1 -> "(Not " ^ (ppexpr e1) ^ ")"
   | Plus (e1, e2) -> "(" ^ (ppexpr e1) ^ " + " ^ (ppexpr e2) ^ ")"
   | Minus (e1, e2) -> "(" ^ (ppexpr e1) ^ " - " ^ (ppexpr e2) ^ ")"
   | Equal (e1, e2) -> "(" ^ (ppexpr e1) ^ " = " ^ (ppexpr e2) ^ ")"
   | If(e1, e2, e3) -> "If " ^ (ppexpr e1) ^ 
                       " Then " ^ (ppexpr e2) ^
                       " Else " ^ (ppexpr e3)
   | Function(Ident(x), t, e) -> "(Fun (" ^ x ^ " : " ^ pptau t ^ ") . " ^ (ppexpr e) ^ ")"
   | Fix(Ident(z), Ident(x), t1, t2, e) -> 
        "(Fix " ^ z ^ " . (" ^ x ^ " : " ^ pptau t1 ^ ") : " ^ pptau t2 ^ " . " ^ (ppexpr e) ^ ")"
   | Let(Ident(x), e1, e2) -> "Let " ^ x ^ " = " ^ (ppexpr e1) ^ " In\n" ^ (ppexpr e2)
   | Appl(e1, e2) -> (ppexpr e1) ^ " " ^ (ppexpr e2)
   | Pair(e1, e2) -> "(" ^ (ppexpr e1) ^ ", " ^ (ppexpr e2) ^ ")"
   | Fst(e1) -> 
      (match e1 with Pair(_) -> "Fst" ^  (ppexpr e1) 
                  | _ ->  "Fst(" ^  (ppexpr e1) ^ ")")
   | Snd(e1) -> 
      (match e1 with Pair(_) -> "Snd" ^  (ppexpr e1) 
                  | _ ->  "Snd(" ^  (ppexpr e1) ^ ")")
   | Gen(Ident(x), e) -> "(Gen " ^ x ^ " . " ^ ppexpr e ^ ")"
   | Inst(e, t) -> ppexpr e ^ "<" ^ pptau t ^ ">"


(*
   pptyping : expr -> unit
   in : An expression AST [[e]].
   out : unit.
   side effects: prints "e : tau" to stdout if e : tau is valid, otherwise raises TypeError
*) 
let pptyping e = let t = typing e in print_string (ppexpr e ^ " : " ^ pptau t ^ "\n")
