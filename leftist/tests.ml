open Leftist;;

(* Returns true if funtion throws an exception 'Empty', false otherwise. *)
let debug_throws_empty f q =
  try f q ; false 
  with Empty -> true

(* Inicialize random seed. *)
let _ = Random.self_init ();;

(* Different types random generators. NOTE: _ argument is necessary, because
   it is then passed to another function as a 'parameterless' function not as 
   a constant. *)
let generate_int _ = Random.int 1000000
let generate_float _ = Random.float 1.
let generate_char _ = Char.chr (97 + Random.int 26)
let generate_string max_len _ = 
  let rec make_str left_length a = 
    if left_length = 0 then a
    else make_str (left_length -1) (String.make 1 (generate_char ())::a) 
  and length = Random.int max_len + 1
  in 
  String.concat "" (make_str length []) 

let debug_test_with_elements input_size random_generator = 
  let create_random_list length = 
    let rec create_list elements_left a = 
      if elements_left = 0 then a
      else 
        create_list (elements_left - 1) (random_generator () :: a)
    in create_list length [] |> List.rev
  and make_pq_from_list l =
    let rec make_pq l queue = 
      match l with
      | [] -> queue
      | h::t -> make_pq t (add h queue)
    in make_pq l empty
  and pop_from_pq pq size = 
    let rec pop pq size a =
      if size = 0 then
        if debug_throws_empty delete_min pq then a
        else
          failwith ("Wrong! Queue should be empty but exception " ^
                    "'Empty' was not throwed!\n")
      else
        let min_element, new_pq = delete_min pq
        in
        pop new_pq (size - 1) (min_element::a)
    in
    pop pq size [] |> List.rev
  and equal_lists l1 l2 =
    let rec eq_list l1 l2 =
      match l1, l2 with
      | [], [] -> true
      | [], _ -> false
      | _, [] -> false
      | h1::t1, h2::t2 -> h1 = h2 && eq_list t1 t2
    in
    eq_list l1 l2 
  in 
  let input_list = create_random_list input_size 
  in
  let sorted_list = List.sort 
      (fun a b -> if a < b then -1 else if a > b then 1 else 0) input_list
  and result_list = pop_from_pq (make_pq_from_list input_list) input_size
  in
  if equal_lists sorted_list result_list then ()
  else failwith "Wrong answer!\n";;

let () = debug_test_with_elements 500000 generate_float;;
let () = debug_test_with_elements 500000 generate_int;;
let () = debug_test_with_elements 800 generate_char;;
let () = debug_test_with_elements 400 (generate_string 20);;

let pq1 = empty;;
let pq1 = add 5 pq1;;
let pq1 = add 10 pq1;;
let pq1 = add 12 pq1;;

let pq2 = empty;;
let pq2 = add 14 pq2;;
let pq2 = add 7 pq2;;
let pq2 = add 3 pq2;;
let pq2 = add 8 pq2;;

let pq3 = join pq1 pq2;;

let pop_val, pq3 = delete_min pq3;; 
assert (pop_val = 3)
let pop_val, pq3 = delete_min pq3;; 
assert (pop_val = 5)
let pop_val, pq3 = delete_min pq3;; 
assert (pop_val = 7)
let pop_val, pq3 = delete_min pq3;; 
assert (pop_val = 8)
let pop_val, pq3 = delete_min pq3;; 
assert (pop_val = 10)
let pop_val, pq3 = delete_min pq3;; 
assert (pop_val = 12)
let pop_val, pq3 = delete_min pq3;; 
assert (pop_val = 14);
assert (debug_throws_empty delete_min pq3)

let pq4 = join empty empty;;
assert (debug_throws_empty delete_min pq4)
let pq4 = pq4 |> add 1 |> add 6 |> add 6 |> add 6 |> add 6 |> add 1 |> add 1 
          |> add 1 |> add 1 |> add 1;;
let pq6 = join pq4 pq1
let pop_val, pq4 = delete_min pq4;; assert (pop_val = 1)
let pop_val, pq4 = delete_min pq4;; assert (pop_val = 1)
let pop_val, pq4 = delete_min pq4;; assert (pop_val = 1)
let pop_val, pq4 = delete_min pq4;; assert (pop_val = 1)
let pop_val, pq4 = delete_min pq4;; assert (pop_val = 1)
let pop_val, pq4 = delete_min pq4;; assert (pop_val = 1)
let pop_val, pq4 = delete_min pq4;; assert (pop_val = 6)
let pop_val, pq4 = delete_min pq4;; assert (pop_val = 6)
let pop_val, pq4 = delete_min pq4;; assert (pop_val = 6)
let pop_val, pq4 = delete_min pq4;; assert (pop_val = 6);
assert (debug_throws_empty delete_min pq4)

let pq5 = empty |> add "aaa" |> add "aab" |> add "aac" |> add "a" |> add "ab"
let pop_val, pq5 = delete_min pq5;; assert (pop_val = "a")
let pop_val, pq5 = delete_min pq5;; assert (pop_val = "aaa")
let pop_val, pq5 = delete_min pq5;; assert (pop_val = "aab")
let pop_val, pq5 = delete_min pq5;; assert (pop_val = "aac")
let pop_val, pq5 = delete_min pq5;; assert (pop_val = "ab");
assert (debug_throws_empty delete_min pq5)

let pop_val, _ = delete_min pq6;; assert (pop_val = 1)

let pq7 = empty |> add 1 |> delete_min |> snd |> add 1 |> delete_min |> snd
          |> add 1 |> delete_min |> snd |> add 1 |> delete_min |> snd |> add 1 
          |> delete_min |> snd |> add 1 |> delete_min |> snd;;
assert (debug_throws_empty delete_min pq7);

Printf.printf "Testing completed successfully. Have a nice day. :)\n"