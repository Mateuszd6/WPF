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
  else raise Not_found
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
      else (assert (get_height_difference left_subt >= 0) ; left_subt)
    in
    (* Wykonujemy rotacje na otrzymanych poddrzwach. *)
    rotate_left (make_set left_subt root_val right_subt)
    (* Analogicznie postępujemy gdy drzewo przeciazone w prawo. *)
  else if height_difference <= -2 then 
    let right_subt = 
      if get_height_difference right_subt > 0 
      then rotate_left right_subt
      else (assert (get_height_difference right_subt <= 0) ; right_subt)
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
  let x = if found_left then (assert (left_x < x) ; left_x) else x
  and y = if found_right then (assert (right_y > y) ; right_y) else y
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

(* ------------------------------------------------------------------------- *)
(* ------------------------------------------------------------------------- *)
(* ------------------------------------------------------------------------- *)
(* ------------------------------------------------------------------------- *)
(* ------------------------------------------------------------------------- *)
(* ------------------------------------------------------------------------- *)
(* ------------------------------------------------------------------------- *)
(* ------------------------------------------------------------------------- *)
(* ------------------------------------------------------------------------- *)
(* ------------------------------------------------------------------------- *)
(* ------------------------------------------------------------------------- *)
(* ------------------------------------------------------------------------- *)
(* ------------------------------------------------------------------------- *)
(* ------------------------------------------------------------------------- *)
(* ------------------------------------------------------------------------- *)
(* ------------------------------------------------------------------------- *)

(* Returns false if BST property is not fullfield, or if there are 
   two intervals that should be merge into one such as [1-4] [5-6]. *)
let debug_is_bst set =
  (* Takes two intervals left and right one and makes sure they do not overlap
     and the difference between left and right one is <= 1. *)
  let comapre_subtrees left_set right_set = 
    match left_set, right_set with
    | _, Nil | Nil, _ -> true
    | Node(_, (beg_left, end_left), _, _, _), 
      Node(_, (beg_right, end_right), _, _, _) -> 
      if end_left > beg_right || end_left + 1 = beg_right 
      then false
      else true
  in
  (* Checks the BST property. If subtree is null true is returned. *)
  let rec is_subtree_bst set =
    match set with 
    | Nil -> true (* Empty tree is a BST. *) 
    | Node(left_subt, (x, y), right_subt, _, _) ->
      (* Current node has BST property... *)
      comapre_subtrees left_subt set 
      && comapre_subtrees set right_subt  
      (* ... and both subtrees have BST property. *)
      && is_subtree_bst left_subt 
      && is_subtree_bst right_subt
  in
  is_subtree_bst set

(* Returns false if given tree does not fulfill AVL tree property,
   which is for every node the height difference of its subtrees is
   not greater than 1. *)
let debug_is_avl set = 
  let rec is_avl_subtree = function
    | Nil -> true
    | Node(left_subt, _, right_subt, height, _) ->
      (* Current node has AVL proprty...*)
      abs (get_height left_subt - get_height right_subt) <= 1 
      (* ... and both subtrees have AVL property. *)
      && is_avl_subtree right_subt
      && is_avl_subtree left_subt
  in
  is_avl_subtree set

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

let debug_ok set = 
  debug_is_avl set && debug_is_bst set && debug_number_of_elem_ok set

let test1 = empty ;;
assert (debug_ok test1);;
let test1 = add (10, 15) test1;;
assert (debug_ok test1);;
let test1 = add (20, 25) test1;;
assert (debug_ok test1);;
let test1 = add (40, 40) test1;;
assert (debug_ok test1);;
let test1 = add (60, 90) test1;;
assert (debug_ok test1);;
let test1 = add (150, 200) test1;;
assert (debug_ok test1);;
let test1 = add (-20, -10) test1;; 
assert (debug_ok test1);;
let test1 = add (-50, -40) test1;;
assert (debug_ok test1);;
let test1 = add (-80, -70) test1;;
assert (debug_ok test1);;

let foo = iter (fun (a, b) -> Printf.printf "[%d;%d] " a b) test1;;
print_string "\n";;
let el = elements test1;;
let el2 = (1,2)::el;;

assert (debug_ok test1);;

let test1 = add (-60, 78) test1;;
let test1 = add (145, 149) test1;;
let test1 = add (92, 95) test1;;
assert (debug_ok test1);;

let foo = iter (fun (a, b) -> Printf.printf "[%d;%d] " a b) test1;;
print_string "\n";;

let bar = split (200) test1;;

let fst (a, _, _) = a;;
let snd (_, a, _) = a;;
let thd (_, _, a) = a;;

let se = bar |> thd;;
let fe = bar |> fst;;

assert (debug_ok fe && debug_ok se);;

print_string (string_of_bool (bar |> snd) ^ "\n");;

(* The number of elements in nodes might be wrong because ints rotate! 
   (We could have tried adding min_int-1 which is max_int). *)
let test2 = empty |> add (4, 6) |> add (min_int+1, -1);;

(* This works on the same trick and above in a 'below' function. *)
test2 |> below 5 |> string_of_int |> print_string;;

assert (debug_ok test2);;

let test3 = empty |> add (min_int, max_int) |> remove (-30,30) |> add (-100, -100) ;;

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


let foo = 3 |> ( * ) 3;;

if foo > 4 then
  print_string "\nHeyah!\n";;


(* =========================*)
(* =========================*)
(* =========================*)
(* =========================*)
(* =========================*)
(* =========================*)
(* =========================*)
(* =========================*)
(* =========================*)
(* =========================*)
(* =========================*)
(* =========================*)
(* =========================*)
(* =========================*)
(* =========================*)
(* =========================*)
(* =========================*)
(* =========================*)
(* =========================*)
(* =========================*)
(* =========================*)
(* =========================*)
(* =========================*)
(* =========================*)
(* =========================*)
(* =========================*)
(* =========================*)
(* =========================*)
(* =========================*)
(* =========================*)

(* Copyright Artur "mrowqa" Jamro 2015 *)
(* Copyright Marcin Mielniczuk 2015 *)
(* Released under the MIT license *)
(* almost rewritten from scratch, only used the codebase *)
(* You can modify 'n' and 'clear_steps' global parameters below. *)
(* After clear_steps steps, if this parameter is positive, *)
(* tested intervals are cleared. *)
(* If you have bug, please set debug = true to make manual debugging *)
(* possible *)

let debug = false
let verbose = false

module Integers =
struct
  type t = int
  let compare = Pervasives.compare
end

module S = Set.Make(Integers)

let loop f (a,b) s =
  let rec pom a b acc =
    if a = b then (f a acc)
    else pom (a+1) b (f a acc)
  in pom a b s

let print_list l = List.iter (fun a -> Printf.printf "%d " a) l

module IntSet =
struct
  include Set.Make(Integers)
  let add = loop S.add
  let remove = loop S.remove
end

(* let's use global vars *)
let lo = if debug then 0 else -100000
let hi = if debug then 20 else 100000
let range = hi - lo
let clear_step = if debug then 10 else 0
let intset = ref IntSet.empty
let iset = ref empty
let rnd l h = Random.int (h-l+1) + l
let random () = Random.int range + lo

type testAction =
  | TestAdd
  | TestRemove
  | TestSplit
  | TestBelow

let sort (x, y) =
  if x < y then (x, y) else (y, x)

let interval_to_list (a,b) =
  List.rev (loop (fun x l -> x::l) (a,b) [])

let to_int_list ll =
  List.fold_left (fun acc el -> List.rev_append (List.rev acc) (interval_to_list el)) [] ll

let print_intset set =
  print_list (IntSet.elements set)

let print_iset set =
  print_list (to_int_list (elements set))

let print_sets () =
  print_string "\nPseudoSet: "; print_intset !intset;
  print_string "\n     iSet: "; print_iset !iset

let bt () = print_sets(); print_newline()

let get_action () : testAction =
  let a = Random.int 20 in
  if a < 8 then
    TestAdd
  else if a < 12 then
    TestRemove
  else if a < 16 then
    TestSplit
  else
    TestBelow

let test_add () : unit =
  let a, b = (random (), rnd 2 10) in
  intset := IntSet.add (a, a+b) !intset;
  iset := add (a, a+b) !iset;
  Printf.printf "add (%d, %d)... " a (a+b);;

let test_remove () : unit =
  let a, b = (random (), rnd 5 20) in
  Printf.printf "remove (%d, %d)... " a (a+b);
  intset := IntSet.remove (a, a+b) !intset;
  iset := remove (a, a+b) !iset

let test_split () : unit =
  let a = random ()
  and side = Random.int 2 in
  let sidetxt = if side = 0 then "below" else "above" in
  Printf.printf "split %d, taking the ones %s... " a sidetxt;
  let b, c, d = IntSet.split a !intset in
  let bb, cc, dd = split a !iset in
  let t = [| b; d |] and tt = [| bb; dd |] in
  assert (c = cc);
  intset := t.(side);
  iset := tt.(side);;

let test_below () : unit =
  let a = random () in
  Printf.printf "below %d... " a;
  let test = below a !iset
  and b, _, _ = IntSet.split (a+1) !intset in
  let c = S.cardinal b in
  try assert (test = c)
  with Assert_failure x ->
    Printf.printf "\nReturned %d, expected %d\n" test c;
    if debug then bt ();
    raise (Assert_failure(x));;

let check_correctness () : unit =
  if verbose then print_sets ();
  let ints = IntSet.elements !intset in
  let i = to_int_list (elements !iset) in
  begin
    try assert (ints = i)
    with Assert_failure x ->
      if debug then bt ();
      raise (Assert_failure(x))
  end;
  Printf.printf "- OK!\n"; flush stdout;;

let _ =
  if false then
    begin
      Random.self_init ();
      Printf.printf "Starting.\n"; flush stdout;
      let i = ref 0 in
      while true do
        let () =
          if clear_step > 0 && !i mod clear_step = 0 then begin
            Printf.printf "[clear]\n";
            iset := empty;
            intset := IntSet.empty
          end;
          i := !i + 1;
          Printf.printf "%d. " !i;
          match get_action () with
          | TestAdd -> test_add ()
          | TestRemove -> test_remove ()
          | TestSplit -> test_split ()
          | TestBelow -> test_below ()
        in check_correctness ()
      done 
    end