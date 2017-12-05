(* Porownuje dwie listy.*)
let rec equal_lists l1 l2 =
  match l1, l2 with
  | [], [] -> true
  | h1::t1, h2::t2 -> h1 = h2 && equal_lists t1 t2
  | _ -> false;;

open ISet;;

(* Prosty test poprawnosciowy. *)
let test_1_elems = [(-80, -70);(-50, -40);(-20, -10);(10, 15);
                    (20, 25);(40, 40);(50,90);(150,200)];;
let test1 = List.fold_left (fun set interval ->
    set |> add interval) empty test_1_elems;;
assert (equal_lists test_1_elems (elements test1));;
let test1 = test1 |> add (-60, 78) |> add (145, 149) |> add (92, 95);;
assert (equal_lists (elements test1) [(-80,-70);(-60,90);(92,95);(145,200)]);;

(* Test poprawnosci remove szczegolnie przy koncach przedzialow 
   i przedzialow nie instniejacych w secie.*)
let test1 = test1 |> remove (-75, -72) |> remove (93,94) |> remove (200,200)
            |> remove (-300, -250) |> remove (-69, -61);;
assert (equal_lists (elements test1)
          [(-80,-76);(-71,-70);(-60,90);(92,92);(95,95);(145,199)]);;

(* Test poprawnosci funkcji split, szczegolnie na koncach przedzialow. *)
let (left_subt, found, right_subt) = split 199 test1;;
assert(is_empty right_subt);;
assert(found);;
assert(is_empty left_subt |> not && mem 199 left_subt |> not
       && mem 198 left_subt);;
let (left_subt, found, right_subt) = split 92 test1;;
assert (mem 92 left_subt |> not && mem 92 right_subt |> not && found);;
assert (equal_lists (elements left_subt) [(-80,-76);(-71,-70);(-60,90)] &&
        equal_lists (elements right_subt) [(95,95);(145,199)]);;

(* Testy sprawdzające poprawnosc dzialan niedaleko zakresu inta. 
   Wielkosci przedzialow nie mieszcza sie w incie i powinny byc max_int *)
let test2 = empty |> add (4, 6) |> add (min_int+1, -1);;
(* Bez zastosowania specjalnej arytmetyki inty sie przekrecaja.  *)
assert(test2 |> below 5 = max_int);;
let test3 = empty |> add (min_int, max_int) 
            |> remove (-30,30) |> add (-100, -100)
            |> remove (-50,50) |> add (-10, 10);;

assert (below 120 test3 = max_int);;
assert (mem (-51) test3);;
assert (mem (-50) test3 |> not);;
assert (mem 51 test3);;
assert (mem 50 test3 |> not);;
assert (mem 11 test3 |> not);;
assert (mem 10 test3);;
assert (mem (-10) test3);;
assert (mem max_int test3);;
assert (mem min_int test3);;
assert (below min_int test3 = 1);;
assert (below max_int test3 = max_int);;

(* Check if split respects int_max. *)
let test_4, should_be_true, should_be_empty = split max_int test3;;
assert (is_empty test_4 |> not);;
assert (should_be_true);;
assert (is_empty should_be_empty);;
(* Do the same for min_int. *)
let should_be_empty, should_be_true, test_4 = split min_int test3;;
assert (is_empty test_4 |> not);;
assert (should_be_true);;
assert (is_empty should_be_empty);;

let test5 = add (-min_int, max_int) empty;;
assert (below min_int test5 = 1);;
assert (below max_int test5 = max_int);;

let test6 = empty |> add (15, 20) |> add (8, 10) |> add (-2, 4);;
assert (below (-3) test6 = 0);;
assert (below 0 test6 = 3);;
assert (below 10000 test6 = 16);;
assert(mem 15 test6);;
assert(mem 20 test6);;
assert(mem 14 test6 |> not);;
assert(mem 21 test6 |> not);;
assert(mem 8 test6);;
assert(mem 10 test6);;
assert(mem 7 test6 |> not);;
assert(mem 11 test6 |> not);;
assert (below min_int test6 = 0);;

let test6 = empty |> add (-1, 0) |> add (1, max_int);;
assert (below max_int test6 = max_int);;

assert (mem 0 test6);;
assert (mem (-2) test6 |> not);;
assert (mem (-1) test6);;
assert (below min_int test6 = 0);;

(* Duży test wydajnosciowy: *)
Random.self_init ();;
let create_interval_list num = 
  let rec create_interval_list_aux num acc =
    let random_sing () = 
      let s = Random.int 2 in
      if s = 0 then -1
      else 1
    in
    if num > 0 then 
      let a = Random.int 10000 * (random_sing ()) in
      let b = Random.int 400 + a in
      create_interval_list_aux (num-1) ((a, b)::acc)
    else 
      acc
  in
  create_interval_list_aux num [];;

(* Test wydajnosciowy funkcji add, mem i remove. *)
let random_test_elem = create_interval_list 1000000;;
let test7 = List.fold_left (fun set interval ->
    let _ = mem (Random.int 20000 - 10000) set 
    in set |> add interval) empty random_test_elem ;;

(* Dodanie przedzialow przecinajacych sie z duza liczba 
   (lub ze wszystkimi) przedzialow w secie. *)
let test7 = test7 |> add (-8000, 7500) |> add (min_int, max_int);;
(* Zastepujemy jednym, maksymalnym przedzialem. *)
let test7 = List.fold_left (fun set interval ->
    let _ = mem (Random.int 20000 - 10000) set 
    in set |> remove interval) test7 random_test_elem;;
assert (is_empty test7 |> not);; 

print_string "Testing completed!\n";;
