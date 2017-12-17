type point = float * float

type kartka = point -> int
(** Poskładana kartka: ile razy kartkę przebije szpilka wbita w danym punkcie *)

let det (x0, y0) (x1, y1) (x2, y2) =
  (x1 -. x0) *. (y2 -. y0) -. (x2 -. x0) *. (y1 -. y0);;

let x = fst and y = snd;;

let obraz_sym_wzgl_prostej l1 l2 p = 
  let add_vec (x1, y1) (x2, y2) = (x1 +. x2, y1 +. y2)
  and substract_vec (x1, y1) (x2, y2) = (x1 -. x2, y1 -. y2)
  and multiplay_vec (x1, y1) k = (x1 *. k, y1 *. k)
  in
  let rzut_na_prosta p1 p2 p3 = 
    let u = (((p3 |> x) -. (p1 |> x)) *. ((p2 |> x) -. (p1 |> x)) +.
             ((p3 |> y) -. (p1 |> y)) *. ((p2 |> y) -. (p1 |> y))) /. 
            (((p1 |> x) -. (p2 |> x)) *. ((p1 |> x) -. (p2 |> x)) +.
             ((p1 |> y) -. (p2 |> y)) *. ((p1 |> y) -. (p2 |> y)))
    in
    add_vec p1 (multiplay_vec (substract_vec p2 p1) u)
  in 
  let q = rzut_na_prosta l1 l2 p
  in
  add_vec p (multiplay_vec (substract_vec q p) 2.);;

let prostokat (x1, y1) (x2, y2) = 
  fun (px, py) ->
    let between_points a b p = a <= p && p <= b in
    if between_points x1 x2 px && between_points y1 y2 py then begin
      (* Printf.printf "(%f;%f) fits in rect: (%f;%f;) - (%f;%f)\n" px py x1 y1 x2 y2;  *)
      1
    end
    else 0

let kolko (center_x, center_y) radius = 
  fun (px, py) -> 
    if (center_x -. px) *. (center_x -. px) +. 
       (center_y -. py) *. (center_y -. py) <= radius *. radius then 1
    else 0

let zloz (l1_x, l1_y) (l2_x, l2_y) c =
  fun (px, py) -> 
    (* Printf.printf "[P: (%f; %f),        L: (%f;%f),(%f;%f)]\n"  *)
      (* px py l1_x l1_y l2_x l2_y ; *)
    let det_val = det (l1_x, l1_y) (px, py) (l2_x, l2_y)
    in
    if det_val = 0. then begin 
      (* Printf.printf "   On line. Opening card...\n"; *)
      c (px, py)
    end
    else if det_val > 0. then begin  
      (* Printf.printf "   Outside card. 0\n"; *)
      0
    end
    else
      c (px, py) +
      c (obraz_sym_wzgl_prostej (l1_x, l1_y) (l2_x, l2_y) (px, py))

let skladaj point_list c = 
  List.fold_left (fun a (p1, p2) -> zloz p1 p2 a) c point_list
