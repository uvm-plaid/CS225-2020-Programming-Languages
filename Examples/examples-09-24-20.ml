[2,46];;

[(3,4.0);(2,6.1);(5,8.9)];;

[[3;5;3];[2;1];[]];;

[(fun x -> x + 1); (fun x -> x + 2)];;

2::3::5::[];;

let (x::xs) = [2;3;5] in x;;

let rec double_all l = 
	 match l with
	    [] -> []
	  | x::xs -> (2 * x) :: (double_all xs);;
    
let rec float_all l = 
	 match l with
	    [] -> []
	  | x::xs -> (float x) :: (float_all xs);;
    
let rec map f l = 
	 match l with
	    [] -> []
	  | x::xs -> (f x) :: (map f xs);;
    
map (fun x -> 2 * x) [1;2;3;4];;

map (fun x -> float x) [1;2;3;4];;

let double_all = map (fun x -> 2 * x);;

let float_all = map float;;

let graph = [(1.1,3.7);(6.2,9.4);(5.5,3.8)];;

let xcoords l = map (fun (x,y) -> y) l;;

let rec fold s b l = 
  match l with 
     [] -> b
   | (x::xs) -> s(x, fold s b xs);;
       
let sumlist = fold (fun (x,y) -> x + y) 0;;
 
let prodlist = fold (fun (x,y) -> x * y) 1;;

let forall p = fold (fun (x,b) -> p(x) && b) true;;

type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree;;

Node(Node(Leaf,1,Leaf), 4, Node(Node(Leaf,2,Leaf),1,Leaf));;

Node(Node(Leaf,'a',Leaf), 'b', Node(Node(Leaf,'d',Leaf),'r',Leaf));;

let bst = Node(Node(Leaf,1,Leaf), 4, Node(Node(Leaf,5,Leaf),7,Node(Leaf,9,Node(Leaf,10,Leaf))));;   (* Tree with BST property *)

let rec treemap f t =
      match t with 
         Leaf -> Leaf
       | Node(tl,x,tr) -> Node(treemap f tl, (f x), treemap f tr);;
       
let rec treefold s b t = 
      match t with 
         Leaf -> b
       | Node(tl,x,tr) -> s(treefold s b tl, x, treefold s b tr);;
       
let preorder = treefold (fun (l,x,r) -> [x] @ l @ r) [];;
let inorder = treefold (fun (l,x,r) -> l @ [x] @ r) [];;
let postorder = treefold (fun (l,x,r) -> l @  r @ [x]) [];;



