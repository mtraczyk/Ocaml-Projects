(*Michał Traczyk, code reviev Franciszek Biel*)
(*Typ, ktorym reprezentuje kolejke priorytetowa. Sklada sie 
z wartosci w wezle, wysokosci prawej sciezki i dwoch synow tego samego typu.*)
type 'a queue = Node of 'a * int * 'a queue * 'a queue
                | Leaf
(*Funkcja, ktora tworzy pusta kolejke.*)
let empty = Leaf
  
(*Wyjatek, ktory podnosze kiedy program jest pytany
o minimum pustej kolejki.*)
exception Empty 

(*Funkcja, ktora sprawdzam dlugosc prawej sciezki. 
Uzywam jej wiele razy w trakcie laczenia dwoch kolejek.*)
let check_height tree =
  match tree with
  | Leaf -> 0
  | Node (_, h, _, _) -> h + 1 

(*Funkcja, ktora odpowiada za laczenie dwoch kolejek.
Laczenie odbywa sie dokladanie tak, jak jest to opisane
w tresci zadania. W wyniku zwraca kolejke bedaca polaczeniem
dwoch innych.*)  
let rec join first_tree second_tree  =
  match (first_tree, second_tree) with
  | (_, Leaf) -> first_tree
  | (Leaf, _) -> second_tree
  | (Node (v, _, ls, rs), Node (v2, _, ls2, rs2)) -> 
      if v < v2 then
        let subtree = join rs second_tree in
        if check_height subtree > check_height ls then
        Node (v, check_height ls, subtree, ls)
        else
        Node (v, check_height subtree, ls, subtree)
      else
        let subtree = join rs2 first_tree in
        if check_height subtree > check_height ls2 then
        Node (v2, check_height ls2, subtree, ls2)
        else
        Node (v2, check_height subtree, ls2, subtree)

(*Funkcja, ktora do kolejki dodaje nowy, pojedynczy element.*)            
let add v first_tree =
  join first_tree (Node (v , 0, Leaf, Leaf))
                  
(*Funkcja, ktora zwraca minimum z kolejki, oraz 
kolejke, ktora jest pozbawiona wczesniejszego elementu minimalnego.*)
let delete_min tree =
  match tree with
  | Leaf -> raise Empty
  | Node (v, _, ls, rs) -> (v , join ls rs)

(*Funkcja, ktora sprawdza czy kolejka jest pusta.
Zwraca prawde jesli tak jest, w przeciwnym przypadku zwraca falsz.*)
let is_empty tree =
  match tree with
  | Leaf -> true
  | _ -> false 
  

(*let q = add 2 empty;;
let q = add 1 q;;
let q = add (-2) q;;
let q = add 0 q;;
let q = add (-1) q;;
let q = add 5 q;;
let q = add 3 q;;
let q = add 6 q;;
let q = add 4 q;;

let (e, q) = delete_min q;;
assert (e = -2);;

let (e, q) = delete_min q;;
assert (e = -1);;

let (e, q) = delete_min q;;
assert (e = 0);;

let (e, q) = delete_min q;;
assert (e = 1);;

let (e, q) = delete_min q;;
assert (e = 2);;

let (e, q) = delete_min q;;
assert (e = 3);;

let (e, q) = delete_min q;;
assert (e = 4);;

let (e, q) = delete_min q;;
assert (e = 5);;


let (e, q) = delete_min q;;
assert (e = 6);;

assert(is_empty q = true);;*)
