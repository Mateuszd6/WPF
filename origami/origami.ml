(* Author: Mateusz Dudziński (394171)
   Reviewer: Gabriel Bożek
   Zadanie 4: Origami WPF Sem. 2017Z *)

(* Punkt na plaszczyznie. *)
type point = float * float

(* Kartka to funkcja z punktu do liczby kartek w tym punkcie. 
   Jest albo kolem / prostokatem, albo funkcja bedaca zlozeniem 
   wzgl. prostej pewnej kartki. W zaleznosci po ktorej stronie 
   znajduje sie szukany punkt wywolywane sa poprzednie funkcje 
   potencjalnie az do elementarnych kartek co daje zlozonosc 
   wykladnicza.*)
type kartka = point -> int

(* Iloczyn wektorowy. Jego znak mowi o polozeniu punktu wzgl. prostej.*)
let det (x0, y0) (x1, y1) (x2, y2) =
  (x1 -. x0) *. (y2 -. y0) -. (x2 -. x0) *. (y1 -. y0);;

(* Zwraca punktt bedacy obrazem punktu p wzgledem prostej l1, l2 *)
let obraz_sym_wzgl_prostej l1 l2 p = 
  (* Dodanie i odejmowanie wspolrzednych wektora. *)
  let add_vec (x1, y1) (x2, y2) = (x1 +. x2, y1 +. y2)
  and substr_vec (x1, y1) (x2, y2) = (x1 -. x2, y1 -. y2)
  (* Mnozenie wektora razy skalar. *)
  and multipl_vec (x1, y1) k = (x1 *. k, y1 *. k)
  in
  (* Najpierw liczymy rzut punktu p na prosta. *)
  let rzut_na_prosta (x1, y1) (x2, y2) (x3, y3) = 
    let u = ((x3 -. x1) *. (x2 -. x1) +. (y3 -. y1) *. (y2 -. y1)) /. 
            ((x1 -. x2) *. (x1 -. x2) +. (y1 -. y2) *. (y1 -. y2))
    in 
    add_vec (x1, y1) (multipl_vec (substr_vec (x2, y2) (x1, y1)) u)
  in 
  let rzut = rzut_na_prosta l1 l2 p 
  in
  (* Liczymy wektor z p do rzutu symetrii i mnozymy go razy dwa.
     Przesuwamy o tyle punkt p zeby uzyskac obraz symetrii. *) 
  add_vec p (multipl_vec (substr_vec rzut p) 2.);;

let prostokat (x1, y1) (x2, y2) = 
  fun (px, py) ->
    let na_odcinku a b p = a <= p && p <= b in
    if na_odcinku x1 x2 px && na_odcinku y1 y2 py then 1
    else 0

let kolko (center_x, center_y) radius = 
  fun (px, py) -> 
    if (center_x -. px) *. (center_x -. px) +. 
       (center_y -. py) *. (center_y -. py) <= radius *. radius then 1
    else 0

(* Zlozenie kartki wzgledem prostej l1, l2. *)
let zloz l1 l2 kartka =
  fun p -> 
    let det_val = det l1 p l2
    in
    (* "Przebicie dokładnie na prostej powinno zwrócić tyle samo,
       co przebicie kartki przed złożeniem." *)
    if det_val = 0. then kartka p
    (* Przebicie na prawo, po tej stronie nie ma kartki. *)
    else if det_val > 0. then 0
    (* Przebicie tak jak przed zlozeniem + przebicie w punkcie bedacym 
       obrazem symetrii tego punktu wzgl. prostej. *)
    else
      kartka p + kartka (obraz_sym_wzgl_prostej l1 l2 p)

let skladaj lista_puntkow kartka = 
  (* Prosto z definicji... *)
  List.fold_left (fun a (p1, p2) -> zloz p1 p2 a) kartka lista_puntkow
