type point = float * float
type kartka = point -> int

(* Iloczyn wektorowy. Jego znak mowi o polozeniu punktu wzgl. prostej. *)
let det (x0, y0) (x1, y1) (x2, y2) =
  (x1 -. x0) *. (y2 -. y0) -. (x2 -. x0) *. (y1 -. y0);;

(* Aliasy dla lepszej czytelnosci kodu. *)
let x = fst and y = snd;;

let obraz_sym_wzgl_prostej l1 l2 p = 
  let add_vec (x1, y1) (x2, y2) = (x1 +. x2, y1 +. y2)
  and substr_vec (x1, y1) (x2, y2) = (x1 -. x2, y1 -. y2)
  and multipl_vec (x1, y1) k = (x1 *. k, y1 *. k)
  in
  (* Najpierw liczymy rzut punktu p na prosta. *)
  let rzut_na_prosta p1 p2 p3 = 
    let u = (((p3 |> x) -. (p1 |> x)) *. ((p2 |> x) -. (p1 |> x)) +.
             ((p3 |> y) -. (p1 |> y)) *. ((p2 |> y) -. (p1 |> y))) /. 
            (((p1 |> x) -. (p2 |> x)) *. ((p1 |> x) -. (p2 |> x)) +.
             ((p1 |> y) -. (p2 |> y)) *. ((p1 |> y) -. (p2 |> y)))
    in
    add_vec p1 (multipl_vec (substr_vec p2 p1) u)
  in 
  let rzut = rzut_na_prosta l1 l2 p
  in
  (* Liczymy vector z p do rzutu symetrii i mnozymy go razy dwa.
     Przesuwamy o tyle punkt p zeby uzyskac obraz symetrii. *) 
  add_vec p (multipl_vec (substr_vec rzut p) 2.);;

let prostokat (x1, y1) (x2, y2) = 
  fun (px, py) ->
    let between_points a b p = a <= p && p <= b in
    if between_points x1 x2 px && between_points y1 y2 py then 1
    else 0

let kolko (center_x, center_y) radius = 
  fun (px, py) -> 
    if (center_x -. px) *. (center_x -. px) +. 
       (center_y -. py) *. (center_y -. py) <= radius *. radius then 1
    else 0

let zloz (l1_x, l1_y) (l2_x, l2_y) c =
  fun (px, py) -> 
    let det_val = det (l1_x, l1_y) (px, py) (l2_x, l2_y)
    in
    (* "Przebicie dokładnie na prostej powinno zwrócić tyle samo,
       co przebicie kartki przed złożeniem." *)
    if det_val = 0. then c (px, py)
    (* Przebicie na prawo, po tej stronie nie ma kartki. *)
    else if det_val > 0. then 0
    (* Przebicie tak jak przed zlozeniem + przebicie w punkcie bedacym obrazem
       symetrii tego punktu wzgl. prostej. *)
    else
      c (px, py) +
      c (obraz_sym_wzgl_prostej (l1_x, l1_y) (l2_x, l2_y) (px, py))

let skladaj point_list c = 
  (* Prosto z definicji... *)
  List.fold_left (fun a (p1, p2) -> zloz p1 p2 a) c point_list
