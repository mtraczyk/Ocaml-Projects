(*Autor: Michał Traczyk 
  Code Review: Witold Drzewakowski*)

(*Funkcja zmienia wejsciowa tablice par
  na dwie oddzielne.*)
  
let przeksztalc tab n = 
  let rozmiary = Array.make n 0 in
  let kon_stan = Array.make n 0 in
  for i = 0 to n - 1 do
    let (a, b) = tab.(i) in
    rozmiary.(i) <- a;
    kon_stan.(i) <- b
  done;
  rozmiary, kon_stan

(*Algorytm Euklidesa*)

let nwd a b = 
  let rec pom a b =
    if a = 0 then b
    else pom (b mod a) a in
  if a <= b then pom a b 
  else pom b a

(*Funkcja, w ktorej sprawdzam 
  zgodnosc nwd.*)

let sprawdz_nwd tab =
  let (nwd1, nwd2) =
    Array.fold_left (fun (a, b) (p, d) ->
      nwd a p, nwd b d) (0, 0) tab in
  nwd2 mod nwd1 = 0

(*Funkcja, w ktorej sprawdzam czy  
  napotkalem nowy stan.*)

let sprawdz znaleziony stany pom kol kon_stan =
  if not !znaleziony && not (Hashtbl.mem stany pom) then 
  begin
    kol := pom :: !kol;
    Hashtbl.add stany pom true;
    if Hashtbl.mem stany kon_stan then 
      znaleziony := true 
  end

(*Funkcja realizujaca wylewanie.*)

let wylej tab kol stany znaleziony kon_stan n = 
  for i = 0 to n - 1 do
    let pom = Array.copy tab in
    pom.(i) <- 0;
    sprawdz znaleziony stany pom kol kon_stan
  done

(*Funkcja realizujaca dolewanie.*)

let dolej tab rozmiary kol stany znaleziony kon_stan n =
  for i = 0 to n - 1 do
    let pom = Array.copy tab in
    pom.(i) <- rozmiary.(i);
    sprawdz znaleziony stany pom kol kon_stan
  done
  
(*Funkcja realizujaca przelewanie.*)

let przelej tab rozmiary kol stany znaleziony kon_stan n = 
  for i = 0 to n - 1 do 
    let a = tab.(i) in 
    for j = 0 to n - 1 do
      if i <> j then 
      begin 
        let pom = Array.copy tab in
        let b = pom.(j) in
        let c = min rozmiary.(j) (pom.(j) + a) in
        pom.(j) <- c;
        pom.(i) <- pom.(i) - c + b;
        sprawdz znaleziony stany pom kol kon_stan
      end
    done
  done

(*Funkcja, w ktorej przegladam kolejne
  stany i wywoluje na nich operacje 
  opisane w tresci zadania.
  Sprawdzam czy w stanie koncowym znajduje sie
  przynajmniej jedno puste lub pelne naczynie.*)

let przelewanka tab =
  let n = Array.length tab in
  let stany = Hashtbl.create n in 
  let rozmiary, kon_stan = przeksztalc tab n in
  let wynik = ref false in
  let wynik2 = ref false in
  let znaleziony = ref false in
  let warstwa = ref 0 in
  let kol = ref ((Array.make n 0) :: []) in
  let kol2 = ref [] in
  for i = 0 to n - 1 do
    let a, b = tab.(i) in
    if a = b || b = 0 then
      wynik := true
  done;
  if n = 0 then 
    wynik := true;
  Hashtbl.add stany (Array.make n 0) true;
  if Hashtbl.mem stany kon_stan then 
    wynik2 := true; 
  if n <> 0 && not !wynik2 && !wynik then
    wynik := sprawdz_nwd tab;
  if !wynik && not !wynik2 then
    while !kol <> [] || !kol2 <> [] do
      if !warstwa mod 2 = 0 then 
      begin
        List.iter (fun x -> 
          wylej x kol2 stany znaleziony kon_stan n; 
          dolej x rozmiary kol2 stany znaleziony kon_stan n;
          przelej x rozmiary kol2 stany znaleziony kon_stan n) !kol;
        kol := []
      end
      else begin 
        List.iter (fun x -> 
          wylej x kol stany znaleziony kon_stan n; 
          dolej x rozmiary kol stany znaleziony kon_stan n;
          przelej x rozmiary kol stany znaleziony kon_stan n) !kol2;
        kol2 := []
      end;
      incr warstwa;
      if Hashtbl.mem stany kon_stan then 
      begin
        kol := [];
        kol2 := [];
        wynik2 := true
      end;
    done;
  if !wynik && !wynik2 then !warstwa else -1

(*TESTY*)

(*let zle = ref 0
let test n a b =
  let res = przelewanka a in
  if res <> b then begin
    Printf.printf "Zly wynik testu %d!!\n" n;
    incr zle
  end;;

test 1 [||] 0;;
test 2 [|(0, 0);(1, 0);(2, 0);(3, 0)|] 0;;
test 3 [|(5,5);(3,3)|] 2;;
test 4 [|(2,2)|] 1;;
test 5 [|(3,1)|] (-1);;
test 6 [|(3,1);(2,2)|] 2;;
test 7 [|(5,1);(3,0);(2,0)|] 4;;

test 8 [|(6,2);(2,1);(10,3);(12,10)|] (-1);;
test 9 [|(2,2);(6,6);(8,1)|] (-1);;
test 10 [| (42,24); (24,9); (10,0) |] (-1);;
test 11 [| (15,6); (12,11); (0,0) |] (-1);;
test 12 [| (12,10); (20,6) |] (-1);;

test 13 [| (1,0); (2,0); (1,0); (1,1); (1,0); (0,0); (0,0); (0,0); (2,2) |] (2);;
test 14 [| (2,0); (2,0); (2,1); (0,0); (0,0); (0,0); (2,0); (0,0); (2,2); (0,0) |] (-1);;
test 15 [| (2,1); (6,1); (0,0); (4,3) |] (-1);;
test 16 [| (16,13); (2,1) |] (-1);;
test 17 [| (2,2); (0,0); (2,2); (2,0); (1,1); (2,2); (0,0) |] (4);;
test 18 [| (1,0); (1,1); (4,3); (5,2) |] (5);;
test 19 [| (1,0); (6,0); (0,0); (2,1); (2,0); (0,0) |] (2);;

test 20 [| (2,1); (4,3); (4,4); (0,0) |] (-1);;
test 21 [| (4,2); (2,2); (4,2) |] (4);;
test 22 [| (3,1); (2,1); (6,6) |] (6);;
test 23 [| (0,0); (1,1); (2,0); (0,0); (1,1); (1,0); (0,0); (1,0); (1,1); (1,0) |] (3);;
test 24 [| (3,2); (7,0); (7,5) |] (10);;
test 25 [| (1,0); (1,1); (0,0); (0,0); (2,2); (1,0); (1,0); (2,0) |] (2);;

test 26 [| (11,7); (2,1) |] (-1);;
test 27 [| (6,3); (1,0); (6,1) |] (7);;
test 28 [| (2,2); (5,2); (0,0); (1,1); (3,3) |] (4);;
test 29 [| (3,2); (6,5); (4,1) |] (-1);;
test 30 [| (4,4); (8,2) |] (-1);;
test 31 [| (1,0); (7,6); (7,1) |] (3);;

let _ = 
  if !zle <> 0 then 
      Printf.printf "\nBlednych testow: %d...\n" !zle;*)
;;
