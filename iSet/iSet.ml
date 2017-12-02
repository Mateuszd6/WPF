(* Author: Mateusz Dudziński (394171)
   Reviewer: ?
   Zadanie 3: Modyikacja drzew WPF Sem. 2017Z *)

(* Uwaga: drzewem zbalansowanym nazywam drzewo w ktorym dla kazdego
   wierzcholka roznica wysokosci poddrzew zaczepionych w tym wierzcholku < 2 *)

(* Zmieniam operator dodawania tak by inty nie przekrecaly sie. Tzn jesli
   suma jest wieksza niz max_int (mniejsza niz min_int)
   to zwracam int_max (int_min). *)
let (+) a b =
  if (a < 0 && b >= 0) || (a >= b && b < 0) then a + b
  else if a >= 0 && b >= 0 then
    if a + b < min a b then max_int
    else a + b
  else if a < 0 && b < 0 then
    if a + b > max a b then min_int
    else a + b
  else failwith "Unexpected behaviour in addition."
let (-) a b =
  a + (-b)

type t =
  (* Pusty zbior. *)
  | Nil
  (* Node: lewe poddrzewo, przedzial w korzeniu, prawe poddrzewo,
     glebokosc drzewa (najdluzsza siezka prosta z korzenia w dol),
     liczba elementow (nie przedzialow ale elementow w tych przedzialach). *)
  | Node of t * (int * int) * t * int * int

(* Wysokosc pustego drzewa jest 0. Dla niepustych drzew
   trzymam wysokosc w reprezentacji. *)
let get_height = function
  | Nil -> 0
  | Node(_, _, _, height, _) -> height

(* Liczba elementow we wszystkich przedzialach zbioru. Rowniez trzymam w
   reprezentacji. *)
let get_numb_elements = function
  | Nil -> 0
  | Node(_, _, _, _, number) -> number

(* Asercja: left_subt, right_subt to drzewa zbalansowane.
   Tworze nowe drzewo, licze jego wysokosc i liczbe elementow
   i jesli trzeba balansuje. Otrzymane w wyniku drzewo tez jest zbanalsowane *)
let make_and_bal left_subt root_val right_subt =
  (* Tworze nowy set i licze wysokosc i liczbe elementow. *)
  let make_set left_subt (root_x, root_y) right_subt =
    Node(left_subt, (root_x, root_y), right_subt,
         max (get_height left_subt) (get_height right_subt) + 1,
         ((root_y - root_x) + 1) + (get_numb_elements left_subt) +
         get_numb_elements right_subt)
  in
  (* Lewa rotacja drzewa AVL.
     Asercja: drzewo przekazane funckji posiada niepuste lewe poddrzewo.
     Wynikiem jest zbalansowane drzewo. *)
  let rotate_left = function
    | Node(Node(left_left_subt, left_root_val, left_right_subt, _, _),
           root_val, right_subt, _, _) ->
      make_set left_left_subt left_root_val
        (make_set left_right_subt root_val right_subt)
    | _-> failwith "Assertion broken. Given tree does not have left subtree"

  (* Prawa rotacja drzewa AVL.
     Asercja: drzewo przekazane funckji posiada niepuste prawe poddrzewo.
     Wynikiem jest zbalansowane drzewo. *)
  and rotate_right = function
    | Node(left_subt, root_val,
           Node(right_left_subt, right_root_val, right_right_subt, _, _),
           _, _) ->
      make_set (make_set left_subt root_val right_left_subt)
        right_root_val right_right_subt
    | _-> failwith "Assertion broken. Given tree does not have right subtree"
  (* Zwaraca roznice wysokosci poddrzew danego zbioru lub 0 gdy zb. pusty.
     Od wys. lewego poddrzewa odejmujemy wys. prawego. *)
  and get_height_difference = function
    | Nil -> 0
    | Node(left_subt, _, right_subt, _, _) ->
      get_height left_subt - get_height right_subt
  in
  let height_difference = get_height left_subt - get_height right_subt in
  (* Jesli drzwo jest przeciazone w lewo... *)
  if height_difference >= 2 then
    let left_subt =
      (* Jesli lewe poddrzewo jest plytsze niz prawe obracamy dla wygody. *)
      if get_height_difference left_subt < 0 then rotate_right left_subt
      else left_subt
    in
    (* Wykonujemy rotacje na otrzymanych poddrzwach. *)
    rotate_left (make_set left_subt root_val right_subt)
    (* Analogicznie postępujemy gdy drzewo przeciazone w prawo. *)
  else if height_difference <= -2 then
    let right_subt =
      if get_height_difference right_subt > 0
      then rotate_left right_subt
      else right_subt
    in
    rotate_right (make_set left_subt root_val right_subt)
    (* W przeciwnym wypadku drzewo jest zbalansowane i nie wymaga rotacji. *)
  else
    make_set left_subt root_val right_subt

(* Znajdujemy przedzial zawierajacy [k]. Wiemy, ze jesli drzewo jest poprawne,
   wzgl. specyfikacji to jest dokladnie jeden taki przedzial. Zwraca
   [(found, interval)] gdzie found = true gdy znalezlismy przedzial w ktorym
   zawiera sie k, false wpp. interval to wartosc przedzialu jesli znaleziony. *)
let rec find_containing k = function
  | Nil -> false, (0, 0)
  | Node(left_subt, (root_x, _), _, _, _) when k < root_x ->
    find_containing k left_subt
  | Node(_, (_, root_y), right_subt, _, _) when k > root_y ->
    find_containing k right_subt
  | Node(_, root_interval, _, _, _) -> true, root_interval

(* Istotne zalozenie: wstawiam przedzial niepokrywajacy sie z zadnym
   przedzialem znajdujacym sie juz w secie. Do seta nie naleza rowniez
   x-1 oraz y+1. Funkcja wstawia przedzial uzywajac standardowej metody
   wstawiania do drzew BST, mocno bazujac na zalozeniu powyzej. *)
let rec add_simple (x, y) = function
  | Nil -> make_and_bal Nil (x, y) Nil
  | Node (left_subt, (i_beg, i_end), right_subt, _, _) ->
    if i_end + 1 < x then
      let right_subt = add_simple (x, y) right_subt
      in make_and_bal left_subt (i_beg, i_end) right_subt
    else if i_beg - 1 > y then
      let left_subt = add_simple (x, y) left_subt
      in make_and_bal left_subt (i_beg, i_end) right_subt
    else failwith ("Assertion broken. (x-y) overlap with an" ^
                   " interval existing already in the set.")

(* Tworzy pusty set. *)
let empty = Nil

(* Sprawdza czy set jest pusty. *)
let is_empty set =
  set = Nil

(* Oblicza akumulator robiac folda w kolejnosci infixowej. *)
let rec fold f set acc =
  match set with
  | Nil -> acc
  | Node(left_subt, root_val, right_subt, _, _) ->
    fold (f) right_subt ((f) root_val (fold (f) left_subt acc))

(* Zwraca rosnaca liste przedzialow. *)
let elements set = fold (fun x a -> (x::a)) set [] |> List.rev

(* Wywoluje funckje w kolejnosci infixowej po wszystkich elemntach drzewa. *)
let iter f set = fold (fun x _ -> f(x) ; ()) set ()

(* true wtw gdy istnieje przedzial w zbiorze do ktorego nalezy k. *)
let rec mem k set = find_containing k set |> fst

(* Usuwam wszystkie elemnty (x, y) ze zbioru przedzialow/ Istniaja maksymalnie
   dwa przedzialy pzecinajace sie z (x, y) ale nie zawierajace sie. Wartosci
   tych przedzialow zostana zmodyfikowane i beda one wstawione na nowo. *)
let rec remove (x, y) set =
  (* Usuwam najmniejszy przedzial ze zbioru. Zwracam wartosc tego przedzialu
     oraz nowe zbalansowane drzewo bez tego elementu. *)
  let rec remove_min_element = function
    | Node(Nil, value, right_subt, _, _) -> value, right_subt
    | Node(left_subt, value, right_subt, _, _) ->
      let min_el, new_left_subt = remove_min_element left_subt
      in
      min_el, make_and_bal new_left_subt value right_subt
    | _ -> failwith "Min element does not exist in the tree!"

  (* Zalozenie: przedzial (x, y) znajduje sie w secie (a x-1 i y+1 nie).
     Usuwa zadany przedzial ze zbioru. Zwracane drzewo jest zbalansowane. *)
  and remove_one (x, y) = function
    | Node(left_subt, (root_x, root_y), right_subt, _, _) ->
      (* Znalezlismy szukany przedzial usuwany go i tworzymy nowe drzewo
         z dwoch poddrzew obecnego. *)
      if root_x = x && root_y = y then
        if right_subt = Nil then left_subt
        else
          (* Usuwamy najmniejszy element z prawego poddrzewa.
             Jest on nowym korzeniem. *)
          let min_el, new_right_subt = remove_min_element right_subt in
          make_and_bal left_subt min_el new_right_subt
      else if root_x > y then
        make_and_bal (remove_one (x, y) left_subt)
          (root_x, root_y) right_subt
      else if root_y < x then
        make_and_bal left_subt (root_x, root_y)
          (remove_one (x, y) right_subt)
      else
        failwith ("Given interval does not fully overlap with" ^
                  " an interval found in the tree.")
    | _ -> failwith "Given interval does not exists in the tree!"
  (* Znajduje jakikolwiek przedzial pokrywajacy sie z (x, y). *)
  and found_any_interpol = function
    | Nil -> false, (0,0)
    | Node(_, (_, root_y), right_subt, _, _) when root_y < x ->
      found_any_interpol right_subt
    | Node(left_subt, (root_x, _), _, _, _) when root_x > y ->
      found_any_interpol left_subt
    | Node(_, (root_x, root_y), _, _, _) -> true, (root_x, root_y)
  in
  let found_lb, lb_val = found_any_interpol set in
  if found_lb then
    let new_set = remove (x, y) (remove_one lb_val set)
    and add_left_bound = add_simple (fst lb_val, x-1)
    and add_right_bound = add_simple (y+1, snd lb_val)
    in
    if fst lb_val < x && snd lb_val > y then
      new_set |> add_left_bound |> add_right_bound
    else if fst lb_val < x then
      new_set |> add_left_bound
    else if snd lb_val > y then
      new_set |> add_right_bound
    else
      new_set
  else
    set

(* Dodaj przedzial (x-y) do zbioru. Najpierw wykonywana jest operacja
   remove (x,y) by dodanie tego przedzialu mozliwe bylo za pomoca operacji
   add_simple. *)
let add (x, y) set =
  (* Jesli x-1(y+1) znajduje sie w zbiorze to wstawiany przedzial nalezy
     rozszerzyc z lewej(prawej) strony. *)
  let found_left, (left_x, _) = find_containing (x-1) set
  and found_right, (_, right_y) = find_containing (y+1) set
  in
  let x = if found_left then left_x else x
  and y = if found_right then right_y else y
  in
  set |> remove (x, y) |> add_simple (x, y)

(* Zwraca krotke set elemtnow mniejszych, bool czy istnieje w secieoraz set
   elemetnow wiekszych wzgledem zmiennej split_with. *)
let rec split split_with = function
  | Nil -> Nil, false, Nil
  | Node (left_subt, (root_x, root_y), right_subt, _, _) ->
    if split_with < root_x then
      let split_left, present, split_right =
        split split_with left_subt
      in
      split_left, present, make_and_bal split_right (root_x, root_y) right_subt
    else if split_with > root_y then
      let split_left, present, split_right =
        split split_with right_subt
      in
      make_and_bal left_subt (root_x, root_y) split_left, present, split_right
    else
      let new_left_subt =
        if split_with = root_x then left_subt
        else left_subt |> add_simple (root_x, split_with-1)
      and new_right_subt =
        if split_with = root_y then right_subt
        else right_subt |> add_simple (split_with+1, root_y)
      in
      new_left_subt, true, new_right_subt

(* Liczba elementow <= k. Dzieki zastosowaniu zmienionych operatorow
   Nie ma problemow z wyjsciem poza zakres typu int. *)
let rec below k = function
  | Nil -> 0
  | Node(left_subt, (root_x, _), _, _, _)
    when k < root_x -> below k left_subt
  | Node(left_subt, (root_x, root_y), right_subt, _, _)
    when k > root_y ->
    get_numb_elements left_subt + (root_y - root_x) + 1
    + below k right_subt
  | Node(left_subt, (root_x, _), _, _, _) ->
    get_numb_elements left_subt + (k - root_x) + 1


(********** DEBUG **********)
(* Sprawdza wlasnosc drzewa BST dla kazdego wierzcholka oraz to czy nie
   ma dwoch przedzialow rozniacych sie o 1 np [1-4] [5-6]. *)
let debug_is_bst set =
  let comapre_subtrees left_set right_set =
    match left_set, right_set with
    | _, Nil | Nil, _ -> true
    | Node(_, (beg_left, end_left), _, _, _),
      Node(_, (beg_right, end_right), _, _, _) ->
      if end_left > beg_right || end_left + 1 = beg_right
      then false
      else true
  in
  let rec is_subtree_bst set =
    match set with
    | Nil -> true (* Empty tree is a BST. *)
    | Node(left_subt, (x, y), right_subt, _, _) ->
      (* Obecny wierzcholek ma wlasnosc BST... *)
      comapre_subtrees left_subt set
      && comapre_subtrees set right_subt
      (* ... I korzenie jego poddrzew tez maja. *)
      && is_subtree_bst left_subt
      && is_subtree_bst right_subt
  in
  is_subtree_bst set

(* Sprawdza własnosc zbalansowanego drzewa dla kazdego wierzcholka. *)
let debug_is_avl set =
  let rec is_avl_subtree = function
    | Nil -> true
    | Node(left_subt, _, right_subt, height, _) ->
      (* Obecny wierzcholek ma wlasnosc AVL... *)
      abs (get_height left_subt - get_height right_subt) <= 1
      (* I korzenie jego poddrzew tez maja. *)
      && is_avl_subtree right_subt
      && is_avl_subtree left_subt
  in
  is_avl_subtree set

(* Sprawdza czy licznosci przedzialow sa policznone dobrze. *)
let debug_number_of_elem_ok set =
  let rec elem_ok = function
    | Nil -> get_numb_elements Nil = 0
    | Node(left_subt, root_val, right_subt, _, elem_count) ->
      elem_ok left_subt
      && elem_ok right_subt
      && elem_count =
         get_numb_elements left_subt + (snd root_val - fst root_val) + 1 +
         get_numb_elements right_subt
  in
  elem_ok set

(* Sprawdza poprawnosc wszystkich wlasnosci drzewa niezbednych
   do jego poprawnego dzialania. *)
let debug_ok set =
  debug_is_avl set && debug_is_bst set && debug_number_of_elem_ok set

(* Porownuje dwie listy.*)
let rec equal_lists l1 l2 =
  match l1, l2 with
  | [], [] -> true
  | h1::t1, h2::t2 -> h1 = h2 && equal_lists t1 t2
  | _ -> false

(* Prosty test poprawnosciowy. *)
let test_1_elems = [(-80, -70);(-50, -40);(-20, -10);(10, 15);
                    (20, 25);(40, 40);(50,90);(150,200)];;
let test1 = List.fold_left (fun set interval ->
    assert (debug_ok set) ; set |> add interval) empty test_1_elems;;
assert (debug_ok test1);;
assert (equal_lists test_1_elems (elements test1));;
let test1 = test1 |> add (-60, 78) |> add (145, 149) |> add (92, 95);;
assert (debug_ok test1);;
assert (equal_lists (elements test1) [(-80,-70);(-60,90);(92,95);(145,200)]);;

(* Test poprawnosci remove szczegolnie przy koncach przedzialow 
   i przedzialow nie instniejacych w secie.*)
let test1 = test1 |> remove (-75, -72) |> remove (93,94) |> remove (200,200)
            |> remove (-300, -250) |> remove (-69, -61);;
assert (debug_ok test1);;
assert (equal_lists (elements test1)
          [(-80,-76);(-71,-70);(-60,90);(92,92);(95,95);(145,199)]);;

(* Test poprawnosci funkcji split, szczegolnie na koncach przedzialow. *)
let (left_subt, found, right_subt) = split 199 test1;;
assert(right_subt = Nil);;
assert(found);;
assert(left_subt = Nil |> not && mem 199 left_subt |> not
       && mem 198 left_subt);;
assert (debug_ok left_subt);;
let (left_subt, found, right_subt) = split 92 test1;;
assert (mem 92 left_subt |> not && mem 92 right_subt |> not && found);;
assert (equal_lists (elements left_subt) [(-80,-76);(-71,-70);(-60,90)] &&
        equal_lists (elements right_subt) [(95,95);(145,199)]);;

(* Testy sprawdzające poprawnosc dzialan niedaleko zakresu inta. 
   Wielkosci przedzialow nie mieszcza sie w incie i powinny byc max_int *)
let test2 = empty |> add (4, 6) |> add (min_int+1, -1);;
assert (debug_ok test2);;
(* Bez zastosowania specjalnej arytmetyki inty sie przekrecaja.  *)
assert(test2 |> below 5 = max_int);;
let test3 = empty |> add (min_int, max_int) 
            |> remove (-30,30) |> add (-100, -100)
            |> remove (-50,50) |> add (-10, 10);;
assert (debug_ok test3);;
assert (below 120 test3 = max_int);;
(* Check if split respects int_max. *)
let test_4, should_be_true, should_be_empty = split max_int test3;;
assert (test_4 |> is_empty |> not);;
assert (should_be_true);;
assert (should_be_empty |> is_empty);;
(* Do the same for min_int. *)
let should_be_empty, should_be_true, test_4 = split min_int test3;;
assert (test_4 |> is_empty |> not);;
assert (should_be_true);;
assert (should_be_empty |> is_empty);;

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
let test6 = List.fold_left (fun set interval ->
    let _ = mem (Random.int 20000 - 10000) set 
    in set |> add interval) empty random_test_elem ;;
(* Wstawienie przedzialu prawdopodobnie pokrywajacego sie
   z duza iloscia przedzialow w drzewie. *)
(assert(debug_ok test6));;
(* Dodanie przedzialow przecinajacych sie z duza liczba 
   (lub ze wszystkimi) przedzialow w secie. *)
let test6= test6 |> add (-8000, 7500) |> add (min_int, max_int);;
(* Zastepujemy jednym, maksymalnym przedzialem. *)
let test6 = List.fold_left (fun set interval ->
    let _ = mem (Random.int 20000 - 10000) set 
    in set |> remove interval) test6 random_test_elem;;
assert (is_empty test6 |> not);;

print_string "Testing completed!\n";;
