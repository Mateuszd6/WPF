(* Author: Mateusz Dudziński (394171)
   Reviewer: Damian Chańko
   Zadanie 3: Modyikacja drzew WPF Sem. 2017Z *)

(* Zmieniam operator dodawania tak by inty nie przekrecaly sie. Tzn jesli
   suma jest wieksza niz max_int (mniejsza niz min_int)
   to zwracam int_max (int_min). *)
let (+) a b =
  if (a < 0 && b >= 0) || (a >= 0 && b < 0) then a + b
  else if a >= 0 && b >= 0 then
    if a + b < min a b then max_int
    else a + b
  else if a < 0 && b < 0 then
    if a + b > max a b then min_int
    else a + b
  else failwith "Unexpected behaviour in addition."

let (-) a b =
  (* Case na przypadek odejmowania min_int'a bo nie mozna go pomnozyc *-1 !*)
  if b = min_int then
    a + max_int + 1
  else
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

(* Uwaga: drzewem zbalansowanym nazywam drzewo w ktorym dla kazdego
   wierzcholka roznica wysokosci poddrzew zaczepionych w tym wierzcholku < 2 *)

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
      (* Jesli lewe poddrzewo lewego drzewa jest plytsze 
         niz prawe poddrzewo lewego drzewa obracamy dla wygody. *)
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
    let new_set = remove_one lb_val set
    and add_left_bound = add_simple (fst lb_val, x-1)
    and add_right_bound = add_simple (y+1, snd lb_val)
    in 
    let new_set = 
      if fst lb_val < x && snd lb_val > y then
        new_set |> add_left_bound |> add_right_bound
      else if fst lb_val < x then
        new_set |> add_left_bound
      else if snd lb_val > y then
        new_set |> add_right_bound
      else
        new_set
    in
    remove (x, y) new_set
  else
    set;;

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

(* Zwraca krotke: set elemtnow mniejszych, bool czy istnieje w secieoraz set
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
