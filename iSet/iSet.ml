type t =
  (* Empty set. *)
  | Nil
  (* left subtree, interval, right subtree, depth of the tree, 
     number of elements (NOTE: not number of intervals, but number of elements 
     in all intervals in the set). *)
  | Node of t * (int * int) * t * int * int;;

let empty = Nil;;

let is_empty set =
  set = Nil;;

let get_height = function
  | Nil -> 0
  | Node(_, _, _, height, _) -> height;;

let get_numb_elements = function
  | Nil -> 0
  | Node(_, _, _, _, number) -> number

let rec fold f set acc =
  match set with 
  | Nil -> acc
  | Node(left_subtree, root_val, right_subtree, _, _) ->
    fold (f) right_subtree ((f) root_val (fold (f) left_subtree acc)) 

let elements set = fold (fun x a -> (x::a)) set [] |> List.rev;; 

let iter f set = fold (fun x _ -> f(x) ; ()) set ();;

let make_and_bal left_subtree root_val right_subtree = 
  let make_set left_subtree root_val right_subtree =
    Node(left_subtree, root_val, right_subtree, 
         max (get_height left_subtree) (get_height right_subtree) + 1, 
         get_numb_elements left_subtree + (snd root_val - fst root_val + 1) 
         + get_numb_elements right_subtree)
  in
  let rotate_left = function
    | Node(Node(left_left_subtree, left_root_val, left_right_subtree, _, _), 
           root_val, right_subtree, _, _) -> 
      make_set left_left_subtree left_root_val 
        (make_set left_right_subtree root_val right_subtree)
    (* TODO: Wyciszenie ostrzerzenia o niewyczerpanym matchu. *)
    | _-> raise Not_found
  and rotate_right = function
    | Node(left_subtree, root_val, 
           Node(right_left_subtree, right_root_val, right_right_subtree, _, _), _, _) -> 
      make_set (make_set left_subtree root_val right_left_subtree)
        right_root_val right_right_subtree 
    (* TODO: Wyciszenie ostrzerzenia o niewyczerpanym matchu. *)
    | _-> raise Not_found
  and get_height_difference = function 
    | Nil -> 0
    | Node(left_subtree, _, right_subtree, _, _) -> 
      get_height left_subtree - get_height right_subtree
  in
  let height_difference = 
    get_height left_subtree - get_height right_subtree
  in
  if height_difference >= 2 then 
    let left_subtree = 
      if get_height_difference left_subtree < 0 
      then rotate_right left_subtree
      else (assert (get_height_difference left_subtree >= 0) ; left_subtree)
    in
    rotate_left (make_set left_subtree root_val right_subtree)
  else if height_difference <= -2 then 
    let right_subtree = 
      if get_height_difference right_subtree > 0 
      then rotate_left right_subtree (* TODO: was rotate_right*)
      else (assert (get_height_difference right_subtree <= 0) ; right_subtree)
    in
    rotate_right (make_set left_subtree root_val right_subtree)
  else
    (* TODO: make_set left_subtree root_val right_subtree
       3x copy-paste. *)
    make_set left_subtree root_val right_subtree

(* TODO: Do I really need it? *)
(* let balance = function
   | Nil -> Nil
   | Node(left_subtree, root_val, right_subtree, _, _) -> 
    make_and_bal left_subtree root_val right_subtree;; *)

let rec find_containing k = function
  | Nil -> false, (0, 0)
  | Node(left_subtree, (root_x, _), _, _, _) when k < root_x -> 
    find_containing k left_subtree
  | Node(_, (_, root_y), right_subtree, _, _) when k > root_y ->
    find_containing k right_subtree
  | Node(_, root_interval, _, _, _) -> true, root_interval;;

let rec mem k set = find_containing k set |> fst;;  

(* Add asumming no interval interpolations! *)
let rec add_simple (x, y) = function
  | Nil -> make_and_bal Nil (x, y) Nil
  | Node (left_subtree, (i_beg, i_end), right_subtree, _, _) ->
    (* Simply insert right. *)
    if i_end + 1 < x then 
      let right_subtree = add_simple (x, y) right_subtree
      in make_and_bal left_subtree (i_beg, i_end) right_subtree
    else if i_beg - 1 > y then 
      let left_subtree = add_simple (x, y) left_subtree
      in make_and_bal left_subtree (i_beg, i_end) right_subtree
    else raise Not_found;;

let rec remove (x, y) set = 
  let rec get_min_element = function 
    | Node(Nil, value, right_subtree, _, _) -> value, right_subtree
    | Node(left_subtree, value, right_subtree, _, _) -> 
      let min_el, new_left_subtree = get_min_element left_subtree
      in
      min_el, make_and_bal new_left_subtree value right_subtree 
    | _ -> failwith "Foo"
  and remove_one (x, y) = function
    | Node(left_subtree, (root_x, root_y), right_subtree, _, _) ->
      if root_x = x && root_y = y then 
        (* Try to take min element from the right subtree if it exist 
           and make it new root. Respects the case when right subtree is null. 
           TODO: Probobly no need to balance, I guess left_subtree is balanced
           already. *)
        if right_subtree = Nil then 
          (* TODO: remove. This asersion will check if balance 
             left_subtree is really required.*)
          (* let is_balanced = function 
             | Nil -> true
             | Node(left_subtree, _, right_subtree, _, _) ->
              abs (get_height left_subtree - get_height right_subtree) <= 1
             in
             assert (is_balanced left_subtree); 
             balance *)
          left_subtree
        else  
          let min_el, new_right_subtree = get_min_element right_subtree
          in 
          make_and_bal left_subtree min_el new_right_subtree
      else if root_x > y then
        make_and_bal (remove_one (x, y) left_subtree)
          (root_x, root_y) right_subtree
      else if root_y < x then
        make_and_bal left_subtree (root_x, root_y) 
          (remove_one (x, y) right_subtree)  
      else
        failwith "Foo"
    | _ -> failwith "Foo"
  and found_any_interpol = function 
    | Nil -> false, (0,0)
    | Node(_, (_, root_y), right_subtree, _, _) when root_y < x ->
      found_any_interpol right_subtree
    | Node(left_subtree, (root_x, _), _, _, _) when root_x > y ->
      found_any_interpol left_subtree
    | Node(_, (root_x, root_y), _, _, _) -> true, (root_x, root_y)
  in
  let found_lb, lb_val = found_any_interpol set
  in
  (* TODO: This looks bad, althoung works! *)
  if found_lb then
    if fst lb_val < x && snd lb_val > y then
      remove (x, y) (remove_one lb_val set
                     |> add_simple (fst lb_val, x-1) |> add_simple (y+1, snd lb_val))
    else if fst lb_val < x then
      remove (x, y) (remove_one lb_val set |> add_simple (fst lb_val, x-1))
    else if snd lb_val > y then
      remove (x, y) (remove_one lb_val set |> add_simple (y+1, snd lb_val))
    else
      remove (x, y) (remove_one lb_val set)
  else
    set;;

let add (x, y) set =
  (* let rec find_overlap (x, y) go_left = function 
    | Node(_, (_, root_y), right_subtree, _, _) when root_y + 1 < x -> 
      find_overlap (x, y) go_left right_subtree
    | Node(left_subtree, (root_x, _), _, _, _) when y + 1 < root_x -> 
      find_overlap (x, y) go_left left_subtree
    | Node(left_subtree, (root_x, root_y), right_subtree, _, _) ->
      if root_x <= y && x <= root_y then
        let loop_found, loop_value = 
          if go_left then find_overlap (x, y) go_left left_subtree
          else find_overlap (x, y) go_left right_subtree
        in
        if loop_found then
          true, loop_value
        else
          true, (root_x, root_y)
      else
      if go_left then find_overlap (x, y) go_left right_subtree 
      else find_overlap (x, y) go_left left_subtree
    | Nil -> false, (0, 0)
  in *)
  let found_left, (left_x, left_y) = (* find_overlap (x-1, y+1) true set *)
    find_containing (x-1) set
  and found_right, (right_x, right_y) = (* find_overlap (x-1, y+1) false set *)
    find_containing (y+1) set
  in
  let x = if found_left (* && left_x <= y *) then min x left_x else x
  and y = if found_right (* && right_y >= y  *) then max y right_y else y
  in
  remove (x, y) set |> add_simple (x, y);;

let rec split split_with = function
  | Nil -> Nil, false, Nil
  | Node (left_subtree, (root_x, root_y), right_subtree, _, _) -> 
    if split_with < root_x then 
      let split_left, present, split_right = 
        split split_with left_subtree
      in
      split_left, present, make_and_bal split_right (root_x, root_y) right_subtree
    else if split_with > root_y then
      let split_left, present, split_right = 
        split split_with right_subtree
      in
      make_and_bal left_subtree (root_x, root_y) split_left, present, split_right
    else 
      let new_left_subtree = 
        if split_with = root_x then left_subtree
        else left_subtree |> add_simple (root_x, split_with-1)
      and new_right_subtree =
        if split_with = root_y then right_subtree
        else right_subtree |> add_simple (split_with+1, root_y)
      in
      new_left_subtree, true, new_right_subtree;;

(* function
   | Nil -> false
   | Node(left_subtree, (root_x, _), _, _, _) when k < root_x -> 
   mem k left_subtree 
   | Node(_, (_, root_y), right_subtree, _, _) when k > root_y ->
   mem k right_subtree
   | Node(_, (_, _), _, _, _) -> true;; *)

let rec below k = function 
  | Nil -> 0
  | Node(left_subtree, (root_x, _), _, _, _) when k < root_x -> 
    below k left_subtree 
  | Node(left_subtree, (root_x, root_y), right_subtree, _, _) when k > root_y ->
    get_numb_elements left_subtree + (root_y - root_x + 1) + below k right_subtree
  | Node(left_subtree, (root_x, _), _, _, _) ->
    get_numb_elements left_subtree + (k - root_x + 1);;

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
    | Node(left_subtree, (x, y), right_subtree, _, _) ->
      (* Current node has BST property... *)
      comapre_subtrees left_subtree set 
      && comapre_subtrees set right_subtree  
      (* ... and both subtrees have BST property. *)
      && is_subtree_bst left_subtree 
      && is_subtree_bst right_subtree
  in
  is_subtree_bst set;;

(* Returns false if given tree does not fulfill AVL tree property,
   which is for every node the height difference of its subtrees is
   not greater than 1. *)
let debug_is_avl set = 
  let rec is_avl_subtree = function
    | Nil -> true
    | Node(left_subtree, _, right_subtree, height, _) ->
      (* Current node has AVL proprty...*)
      (* TODO: Mayby make get_height_diff global? Coz it is used here... *)
      abs (get_height left_subtree - get_height right_subtree) <= 1 
      (* ... and both subtrees have AVL property. *)
      && is_avl_subtree right_subtree
      && is_avl_subtree left_subtree
  in
  is_avl_subtree set;;

let debug_number_of_elem_ok set = 
  let rec elem_ok = function
    | Nil -> get_numb_elements Nil = 0
    | Node(left_subtree, root_val, right_subtree, _, height) ->
      elem_ok left_subtree 
      && elem_ok right_subtree 
      && height = get_numb_elements left_subtree + 
                  (snd root_val - fst root_val + 1) 
                  + get_numb_elements right_subtree
  in
  elem_ok set;;

let debug_ok set = 
  debug_is_avl set && debug_is_bst set && debug_number_of_elem_ok set;;

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

(* let print_debug (a, b) text = 
   " f(" ^ string_of_int a ^ "," ^text ^ ")";;

   let foo = fold (print_debug) test1 "a";;
   print_string ("\n" ^ foo ^ "\n");; *)

(* let test1 = remove (-19, 75) test1;;
   assert (debug_ok test1);; *)

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

print_string (string_of_bool (bar |> snd));

assert (1 > 0);;

(* 
iter (fun x -> Printf.printf "[%d;%d] " (fst x) (snd x)) test1;;
print_string "\n";;

let foo = min_element test1;;

List.fold_left (fun a x -> Printf.printf "[%d;%d] " (fst x) (snd x)) 
  () (elements test1);;
print_string "\n";; *)

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
  iset := tt.(side)

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
    raise (Assert_failure(x))

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
  Printf.printf "- OK!\n"; flush stdout

let _ =
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