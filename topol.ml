(*autor Michał Traczyk*)
(*code review Marcin Brojek*)

(*Wyjatek podnoszony kiedy w zadanym grafie skierowanym
istnieje cykl.*)
exception Cykliczne

(*Funkcja za pomoca, ktorej sprawdzam 
czy jakis wierzcholek byl juz odwiedzony 
w trakcie przechodzenia grafu w glab.*)
let czy_odwiedzone odwiedzone x =
  try Hashtbl.find odwiedzone x with
  Not_found -> true 

(*Funkcja za pomoca, ktorej sprawdzam 
czy jakis wierzcholek byl juz przetworzony
w trakcie przechodzenia grafu w glab.*)  
let czy_przetworzone przetworzone x = 
  try Hashtbl.find przetworzone x with
  Not_found -> true

(*Funkcja, ktora podaje wierzcholki, 
do ktorych prowadzi krawedz z zadanego wierzcholka.*)
let czy_ma_sasiadow sasiedzi x =
  try Hashtbl.find sasiedzi x with
  Not_found -> []

(*Funkcja, ktora odpowiada za przetworzenie 
grafu do formy, w ktorej latwo go pozniej odczytac.*)
let utworz_graf lst sasiedzi = 
  List.iter (fun (a, b) -> Hashtbl.add sasiedzi 
  a (b @ (czy_ma_sasiadow sasiedzi a))) lst;
  sasiedzi

(*Funkcja, ktora odpowiada za sprawdzenie
czy nie zostal napotkany cykl w trakcie 
przechodzenia grafu.*)
let sprawdz odwiedzone przetworzone x =
  if czy_odwiedzone odwiedzone x = false && czy_przetworzone przetworzone x
  then raise Cykliczne
  else if czy_odwiedzone odwiedzone x
  then true else false

(*Dla danej listy [(a_1,[a_11;...;a_1n]); ...; (a_m,[a_m1;...;a_mk])]
zwraca liste, na ktorej kazdy z elementow a_i oraz a_ij wystepuje
dokladnie raz i ktora jest uporzadkowana w taki sposob, ze kazdy
element a_i jest przed kazdym z elementow a_i1 ... a_il*)
let topol lst = 
  let n = List.length lst in
  let sasiedzi = utworz_graf lst (Hashtbl.create n) in
  let odwiedzone = Hashtbl.create n in
  let przetworzone = Hashtbl.create n in
  let wynik = ref [] in
  let rec dfs wierz =
    Hashtbl.add odwiedzone wierz false;
    List.iter (fun x -> if sprawdz odwiedzone przetworzone x 
    then dfs x) (czy_ma_sasiadow sasiedzi wierz);
    Hashtbl.add przetworzone wierz false;
    wynik := wierz :: !wynik
  in
  Hashtbl.iter (fun a _ -> if czy_odwiedzone 
  odwiedzone a then dfs a) sasiedzi;
  !wynik
  
(*exception WA;;

let debug = false;;

(* True if the given order is a correct topological order, *)
(* false otherwise. Checks all edges.                      *)
let test graph order =
  let hashtbl = Hashtbl.create (List.length order)
  in
  List.iteri (fun i x -> Hashtbl.add hashtbl x i) order;
  let check_one (v, l) =
    List.iter (fun u ->
      if (Hashtbl.find hashtbl v) > (Hashtbl.find hashtbl u)
      then raise WA;) l
  in
  try (List.iter check_one graph; true)
  with WA -> false

(* Tests if Topol raises Cykliczne for the given graph *)
let test_cyclic g =
  try let _ = topol g in false
  with Cykliczne -> true

;;
      
if debug then print_endline "Acyclic correctness tests...";;
      
let g = [
  ("1", ["2"; "3"]);
  ("3", ["2"]);
  ("4", ["3"; "2"])
];;

assert(test g (topol g));;

let g = [
  ("first", ["second"; "fourth"; "eighth"]);
  ("second", ["fourth"; "eighth"]);
  ("third", ["fourth"; "fifth"; "sixth"]);
  ("fourth", ["eighth"]);
  ("fifth", ["sixth"; "seventh"]);
  ("sixth", ["eighth"; "first"]);
  ("seventh", ["eighth"]);
];;

assert(test g (topol g));;

let g = [
  (1, [2; 3]);
  (2, [4]);
  (3, [4]);
  (4, [5; 6]);
  (5, [7]);
  (6, [7]);
];;

assert(test g (topol g));;

let g = [
  (1, [7; 2]);
  (3, [4; 2; 1; 7; 5]);
  (4, [2; 7; 1]);
  (5, [7; 4; 1; 2]);
  (6, [1; 3; 2; 5; 4; 7]);
  (7, [2])
];;

assert(test g (topol g));;

let g = [
  (1, [2; 4; 8]);
  (2, [16; 32]);
  (4, [64; 128]);
  (8, [256; 512]);
  (16, [1024]);
  (32, [2048]);
  (64, [4096]);
  (128, [8192]);
  (256, [16384]);
  (512, [32768]);
];;

assert(test g (topol g));;

let g = [
  ("Lorem", ["sit"]);
  ("ipsum", ["sit"; "amet"]);
  ("dolor", ["amet"; "elit"]);
  ("sit", ["consectetur"; "adipiscing"; "elit"]);
];;

assert(test g (topol g));;

let g = [];;

assert(test g (topol g));;

let g = [
  ("through", ["the"; "gates"; "of"; "hell"]);
  ("hell", ["as"; "we"; "make"; "our"; "way"; "to"; "heaven"]);
  ("PRIMO", ["VICTORIA"]);
];;

assert(test g (topol g));;

let g = [
  ("one", ["three"]);
  ("one", ["two"]);
  ("two", []);
  ("two", []);
  ("two", ["three"]);
];;

assert(test g (topol g));;

if debug then print_endline "OK";;

if debug then print_endline "Cyclic correctness tests...";;

let g = [
  (10.001, [10.002]);
  (10.002, [10.001])
];;

assert(test_cyclic g);;

let g = [
  (1, [7; 2; 3]);
  (3, [4; 2; 1; 7; 5]);
  (4, [2; 7; 1]);
  (5, [7; 4; 1; 2]);
  (6, [1; 3; 2; 5; 4; 7]);
  (7, [2])
];;

assert(test_cyclic g);;

let g = [
  (1, [2]);
  (2, [3]);
  (3, [4; 5; 3]);
];;

assert(test_cyclic g);;

let g = [
  ("pole", ["pole"; "lyse"; "pole"])
];;

assert(test_cyclic g);;

let g = [
  ("tu", ["tudu"]);
  ("tudu", ["tudu"; "tudu"; "tudu"])
];;

assert(test_cyclic g);;

if debug then print_endline "OK";;

if debug then print_endline "Marcin Michorzewski's acyclic correctness tests...";;

let g = [
  (11, [12]);
  (12, [13]);
  (7, [8]);
  (8, [9]);
  (1, [2]);
  (13, [6]);
  (3, [4]);
  (5, [6]);
  (6, [7]);
  (10, [11])
];;

assert(test g (topol g));;

let g = [
  (1, [2]);
  (3, [4]);
  (4, [1]);
  (5, [6]);
  (6, [3])
];;

assert(test g (topol g));;

let g = [
  (1, [2; 3; 4]);
  (3, [7; 8]);
  (4, [9; 10]);
  (10, [15; 16]);
  (2, [5; 6]);
  (13, [4; 10]);
  (11, [12]);
  (12, [13; 14])
];;

assert(test g (topol g));;

let g = [
  (1, [2; 3; 4]);
  (3, [7; 8]);
  (4, [9; 10]);
  (10, [15; 16]);
  (2, [5; 6]);
  (13, [4; 10]);
  (11, [12]);
  (12, [13; 14]);
  (15, [16; 8])
];;

assert(test g (topol g));;

let g = [
  (1, [2; 3; 4]);
  (3, [7; 8]);
  (4, [9; 10]);
  (10, [15; 16]);
  (2, [5; 6]);
  (13, [4; 10]);
  (11, [12]);
  (12, [13; 14]);
  (15, [16; 8]);
  (8, [14])
];;

assert(test g (topol g));;

let g = [
  (1, [2]);
  (2, []);
  (3, [2])
];;

assert(test g (topol g));;

let g = [
  ('a', ['e']);
  ('b', ['a'; 'c']);
  ('c', ['a']);
  ('e', [])
];;

assert(test g (topol g));;

if debug then print_endline "OK";;

if debug then print_endline "Marcin Michorzewski's cyclic correctness tests...";;

let g = [
  (3, [4]);
  (5, [6]);
  (6, [7]);
  (10, [11]);
  (11, [12]);
  (12, [13]);
  (7, [8]);
  (9, [13]);
  (8, [9]);
  (1, [2]);
  (13, [6])
];;

assert(test_cyclic g);;

let g = [
  ("Polska", ["Niemcy"]);
  ("Niemcy", ["Polska"])
];;

assert(test_cyclic g);;

let g = [
  (1, [2]);
  (3, [4]);
  (4, [1]);
  (5, [6]);
  (6, [3]);
  (2, [5])
];;

assert(test_cyclic g);;

let g = [
  (1, [2]);
  (3, [4]);
  (4, [1]);
  (5, [6]);
  (6, [3]);
  (1, [5]);
];;

assert(test_cyclic g);;

let g = [
  (1, [2]);
  (3, [4]);
  (4, [1]);
  (5, [6]);
  (6, [3]);
  (2, [6])
];;

assert(test_cyclic g);;

let g = [
  (1, [2]);
  (3, [4]);
  (4, [1]);
  (5, [6]);
  (6, [3]);
  (1, [6])
];;

assert(test_cyclic g);;

let g = [
  (1, [2]);
  (2, [3]);
  (3, [1])
];;

assert(test_cyclic g);;

let g = [
  (1, [2; 3; 4]);
  (3, [7; 8]);
  (4, [9; 10]);
  (10, [15; 16]);
  (2, [5; 6]);
  (13, [4; 10]);
  (11, [12]);
  (12, [13; 14]);
  (15, [16; 8]);
  (8, [12])
];;

assert(test_cyclic g);;

if debug then print_endline "OK";;

if debug then print_endline "==============================================================";;
print_endline "mgienieczko: Performance tests begin, expected time ~30 seconds";;
if debug then print_endline "(correctness is tested as well)";;
if debug then print_endline "==============================================================";;

Random.init 461777389;;

let rnd a b =
  (Random.int (b - a + 1)) + a;;

let shuffle_list l =
  let temp = List.map (fun x -> (Random.bits (), x)) l
  in
  let sorted = List.sort compare temp
  in
  List.map snd sorted
;;

let rand_const = rnd 0 255;;

(* Generates a random string based on passed integer *)
(* Strings have expected length of n / 256           *)
let hash_label n =
  let rec aux acc n =
    if n <= 0 then acc
    else
      let l = (n lxor rand_const) mod 256
      in
      aux (acc ^ (String.make 1 (Char.chr l))) (n - l - 127)
  in
  aux "" (n + 1)
;;

(* Identity *)
let int_label n = n;;

let gen_path f n =
  let labels = Array.init n f
  in
  let rec aux acc v =
    if v = n - 1 then acc
    else aux ((labels.(v), [labels.(v + 1)])::acc) (v + 1)
  in
  aux [] 0
;;

let gen_loop f n =
  let g = gen_path f n
  in
  (f (n - 1), [f 0])::g
;;

let gen_random f n m =
  let labels = Array.init n f
  in
  let edges = Array.make n []
  in
  for i = 0 to m - 1 do
    let v = rnd 0 (n - 2)
    in
    let u = rnd (v + 1) (n - 1)
    in
    edges.(v) <- (labels.(u))::(edges.(v))
  done;
  List.mapi (fun i x -> (labels.(i), x)) (Array.to_list edges)
;;

let gen_random_cyclic f n m =
  let labels = Array.init n f
  in
  let edges = Array.make n []
  in
  for i = 0 to m - 1 do
    let v = rnd 0 (n - 1)
    in
    let u = rnd 0 (n - 1)
    in
    edges.(v) <- (labels.(u))::(edges.(v))
  done;
  List.mapi (fun i x -> (labels.(i), x)) (Array.to_list edges)
;;

if debug then print_endline "Smaller int acyclic performance tests... ";;

let g = shuffle_list (gen_path int_label 20000);;
assert(test g (topol g));;

let g = shuffle_list (gen_random int_label 5000 20000);;
assert(test g (topol g));;

if debug then print_endline "OK";;

if debug then print_endline "Smaller int cyclic performance tests... ";;

let g = shuffle_list (gen_loop int_label 20000);;
assert(test_cyclic g);;

let g = shuffle_list (gen_random_cyclic int_label 5000 20000);;
assert(test_cyclic g);;

if debug then print_endline "OK";;

if debug then print_endline "Smaller string acyclic performance tests...";;

let g = shuffle_list (gen_path hash_label 20000);;
assert(test g (topol g));;

let g = shuffle_list (gen_random hash_label 5000 20000);;
assert(test g (topol g));;

if debug then print_endline "OK";;

if debug then print_endline "Smaller string cyclic performance tests...";;

let g = shuffle_list (gen_loop hash_label 20000);;
assert(test_cyclic g);;

let g = shuffle_list (gen_random_cyclic hash_label 5000 20000);;
assert(test_cyclic g);;

if debug then print_endline "OK";;

if debug then print_endline "Bigger int acyclic performance tests... ";;

let g = shuffle_list (gen_path int_label 90000);;
assert(test g (topol g));;

let g = shuffle_list (gen_random int_label 100000 1000000);;
assert(test g (topol g));;

if debug then print_endline "OK";;

if debug then print_endline "Bigger int cyclic performance tests... ";;

let g = shuffle_list (gen_loop int_label 90000);;
assert(test_cyclic g);;

let g = shuffle_list (gen_random_cyclic int_label 100000 1000000);;
assert(test_cyclic g);;

if debug then print_endline "OK";;

if debug then print_endline "Bigger string acyclic performance tests...";;

let g = shuffle_list (gen_path hash_label 90000);;
assert(test g (topol g));;

let g = shuffle_list (gen_random hash_label 100000 1000000);;
assert(test g (topol g));;

if debug then print_endline "OK";;

if debug then print_endline "Bigger string cyclic performance tests...";;

let g = shuffle_list (gen_loop hash_label 90000);;
assert(test_cyclic g);;

let g = shuffle_list (gen_random_cyclic hash_label 100000 1000000);;
assert(test_cyclic g);;

if debug then print_endline "OK";;

if debug then print_endline "All tests OK";;*)
