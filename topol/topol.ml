(* Author: Mateusz Dudziński (394171)
   Reviewer: Mateusz Rychlicki
   Zadanie 5: Sortowanie topologiczne WPF Sem. 2017Z *)

open PMap

(** wyjatek rzucany przez [topol] gdy zaleznosci sa cykliczne *)
exception Cykliczne

(* Typ odwiedzonego wierzcholka.*) 
type visit_type =
  (* Wierzcholek jest obecnie przetwarzany ale nie jest jeszcze opuszczony. *)
  | Visited  
  (* Wierzcholek zostal juz przetworzony (tzn DFS z niego wyszedl. *)
  | Explored
  (* Wierzcholek jeszcze nie odwiedzony. *)  
  | NotVisited

(* Typ uzyty przy reprezentacji grafu. Grapf reprezentujemy jako mape postaci 
   wartosc (typu 'a) w 'a node. node posiada informacje o tym czy wierzcholek 
   jest przetwarzany (visit_type) oraz liste krawedzi. *)
type 'a node =
  {
    mutable vis_type    : visit_type;
    mutable childs       : 'a list;
  }

(* Buduje graf z listy wierzcholkow takiej jak w specyfikacji zadania. *)
let make_graph l = 
  List.fold_left (fun acc (v, l) -> 
    (* Na wypadek grafow reprezentowanych np [(1, [2]); (1, [3])]. Jesli 
        jakas tablica sasiadow jest juz w mapie to dodajemy do niej nowa 
        liste. Wirzcholki moga sie powtarzac, nie jest to problem. *)
    if PMap.mem v acc then
      let found_node = (PMap.find v acc) in 
      found_node.childs <- l @ found_node.childs;
      acc
    else
      add v { vis_type = NotVisited; childs = l} acc) PMap.empty l

(** Dla danej listy [(a_1,[a_11;...;a_1n]); ...; (a_m,[a_m1;...;a_mk])] 
    zwraca liste, na ktorej kazdy z elementow a_i oraz a_ij wystepuje
    dokladnie raz i ktora jest uporzadkowana w taki sposob, ze kazdy
    element a_i jest przed kazdym z elementow a_i1 ... a_il *)
let topol l =
  let graph = ref (make_graph l)
  and res = ref []
  in
  (* Oznaczamy wierzchołek jako przetwarzany teraz. Wywolujemy sie rekurencyjnie 
     na jego sasiadach, przed wyjsciem dodajemy go do wyniku *)
  let rec dfs value =
    let nod = PMap.find value !graph 
    in
    nod.vis_type <- Visited;
    List.iter process_child nod.childs;
    res := value::(!res);
    nod.vis_type <- Explored
  (* Sprawdz czy mozna odwiedzic i ew. odwiedz wierzcholek [v]. *)
  and process_child v =
    if PMap.mem v !graph then begin
      match (PMap.find v !graph).vis_type with
      (* Gdy trafiliśmy do wierzcholka na przetwarzanego obecnie to cykl. *)
      | Visited -> raise Cykliczne
      | NotVisited -> dfs v
      | Explored -> ()
    end
    (* Jesli wierzcholek nie wystapil jako pierwszy element z pary w wejsciowej
       liscie musimy go dodac do grafu. *)
    else begin
      graph := PMap.add v { vis_type = Explored ; childs = [] } (!graph);
      res := v::(!res)
    end
  in
  (* Idziemy pokolei po wszystkich wierzcholkach i wywolujemy DFS'a na jeszcze 
     nie odwiedzonych. *)
  List.iter 
    (fun x -> if (PMap.find x !graph).vis_type = NotVisited then dfs x)
    (List.map fst l);
  !res

(*
(* --------------------------------- Testy --------------------------------- *)
exception WrongOrder;;
exception IncompleteOutput;;

(* Sprawdza z definicji sortowanie topologicznel; jeśli [A] jest przed [B] w
   sortowaniu topologicznym grafu to wiadomo, że z [B] nie da się dojść do [A].
   Funckja buduje graf i sprawdza w ten sposób wszystkie pary wierzcholkow. 
   Do tego sprawdza czy wszystkie wierzcholki sa w sortowaniu topologicznym. *)
let check l =
  let top_order = topol l
  and graph = ref (List.fold_left (fun acc (v, l) -> 
      add v { vis_type = NotVisited; childs = l} acc) PMap.empty l)
  in 
  let rec dfs v search_for = 
    if PMap.mem v !graph then begin
      (PMap.find v !graph).vis_type <- Visited;
      List.iter 
        (fun x -> 
           if x = search_for then 
             raise WrongOrder 
           else if PMap.mem x !graph then begin
             if (PMap.find x !graph).vis_type = NotVisited then 
               dfs x search_for 
           end
           else
             graph := PMap.add x { vis_type = Visited; childs = [] } (!graph))
        (PMap.find v !graph).childs 
    end
  and check_top_order = function
    | [] | _::[] -> ()
    | h::t -> List.iter (fun x -> dfs x h) t; check_top_order t 
  and check_if_all input_list topol_list = 
    match input_list with
    | [] -> true
    | (h, l)::t -> 
      List.mem h topol_list &&
      List.fold_left (fun a x -> a && List.mem x topol_list) true l
  in
  check_top_order top_order;
  if not (check_if_all l top_order) then raise IncompleteOutput;;

let is_cyclic l =
  try (fun x -> ()) (topol l) ; false  with
  | Cykliczne -> true
  | _ -> false;;

(* Proste przypadki brzegowe: *) 
assert (is_cyclic [('i', ['j']);('j', ['i'])]);;
assert (topol [] = []);;
assert (topol [("Q", [])] = ["Q"]);;
assert (topol [(1, []); (2, []); (2, []); (3, [])] 
  |> List.sort (-) = [1; 2; 3]);;

(* Nie ma zalozenia ze w grafie nie moga wystapic pentle... *)
assert (is_cyclic [(1, [1])]);;
assert (is_cyclic [(1, [1; 2]); (2, [3]); (3, [4])]);;

(* W przypadku rozwiazania ktore zakalada ze element grafu wystepuje co 
   najwyzej raz w liscie jako pierwszy element pary [check] podnosi 
   [IncompleteOutput]. *)
check [(1, [2]); (1, [3])];;

(* Sciezka: *)
let test2 = [(6, [5]); (5, [4]); (4, [3]); (3, [2]); (2, [1])];;
assert (topol test2 = [6; 5; 4; 3; 2; 1]);;

(* Dolaczenie tej krawedzi tworzy cykl: *)
assert (is_cyclic ((1, [6])::test2));;

(* Dwie rozlaczne sciezki: *)
let test3 = test2 @ [(12, [11]); (11, [10]); (10, [9]); (9, [8]); (8, [7])];;
check test3;;

let test4 = [('M', ['A'; 'T']); ('A', ['Z']); ('T', ['E']); 
             ('E', ['Z'; 'U']); ('U', ['S'])];;
check test4;;

let test5 = [("1", ["2"; "3"; "4"; "5"; "6"]); ("2", ["3"; "4"; "5"; "6"]); 
             ("3", ["4"; "5"; "6"]); ("4", ["5"; "6"]); ("5", ["6"])];;
check test5;;

let test6 = [(1, [2]); (2, [3; 4]); (4, [5; 3]); (3, [6])];;
check test6;;

let test7 = [(1, [2]); (2, [4]); (4, [5; 3]); (3, [2; 6])];;
assert (is_cyclic test7);;

let test8 = [(2, [3; 4]); (5, [6]); (4, [5; 6]); (1, [2]);  (3, [4])];;
check test8;;

(* Klika dwudzielna*)
let test9 = [(2, [3; 5]); (5, [6]); (1, [2; 4; 6]); (3, [4; 6]); (4, [5])];;
check test9;;

(* Duzy test: DAG - clique.*)
let aux = Array.init 2500 (fun x -> x) |> Array.to_list;;
let large_test = 
  let rec large_test_aux acc = function 
    | [] -> acc
    | h::t -> large_test_aux ((h, t)::acc) t
  in
  List.tl (large_test_aux [] aux);;

assert (topol large_test = aux);;

print_string "Testing completed!!\n";;
*)