
let rev list = 
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (h :: acc) t
  in
  aux [] list;;

let rec reverse list = match list with
  | [] -> []
  | h :: t -> List.append (reverse t) [h];;
  
rev [1; 2; 3; 4];;
reverse [1; 2; 3; 4];;

let rec last list = match list with
  | [] -> None
  | h :: [] -> Some h
  | _h :: t -> last t;;

last [1; 2; 3; 4];;
last ["a"; "b"; "c"];;

type binary_tree = | Empty | Node of binary_tree * binary_tree

let rec count_leaves tree = match tree with
  | Empty -> 0
  | Node (Empty, Empty) -> 1
  | Node (l, r) -> count_leaves l + count_leaves r;;

count_leaves (Node (Node (Empty, Empty), Empty));;