(* Author: Mateusz Dudziński
   Reviewer: Jan Kociniak
   Zadanie 1: Arytmetyka WPF Sem. 2017Z *)

type wartosc = 
  | Empty                   (* Empty set *)
  | Single of float * float (* <a;b> *)
  | Double of float * float (* (-inf;a> U <b;+inf) *)

(* Redefine float multiplication operator, becasue infinity *. 0. = nan.
   When calculated with new operator the value will be equal 0. *) 
let ( *. ) x y =
  if compare x nan = 0 || compare y nan = 0 then nan
  else if x = 0. || y = 0. then 0.
  else ( *. ) x y

(* Redefine division operator, becasue inf /. inf = nan.
   When using this operator it will be equal +/-inf (depending on the sign). *) 
let ( /. ) x y = 
  if compare x nan = 0 || compare y nan = 0 then nan
  else if x = infinity && y = infinity then infinity
  else if x = infinity && y = neg_infinity then neg_infinity
  else if x = neg_infinity && y = infinity then neg_infinity
  else if x = neg_infinity && y = neg_infinity then infinity
  else x /. y

(**  When using redefined *. and /. operators nan should never be 
     the outcome of any operation. **)

(* This is interval of type double constructor. Returns 
   interval (-inf;a> U <b;+inf) or (-inf;inf) if summed intervals overlap. *)
let double_interval a b =
  if b <= a then Single (neg_infinity, infinity)
  else Double (a, b)

(* Multiplies all values within an interval by -1. Eg.: <a;b> -> <-b;a>,
   <-inf;a> U <b;+inf) -> <-inf;-b> U <-a;+inf) *)
let neg_interval interval = 
  match interval with
  | Empty -> Empty
  | Single (x, y) -> Single (-1. *. y, -1. *. x)
  | Double (x, y) -> Double (-1. *. y, -1. *. x)

(* Sum two intervals. There result can be empty, single interval or 
   double interval. If the outcome is more than two intervals, it means 
   that function was used in wrong context and exception is thrown. *)
let rec interval_sum x y = 
  match (x, y) with
  (* If one interval is empty take the other. *)
  | (Empty, _) -> y
  | (_, Empty) -> x
  | (Single (ax, bx), Single (ay, by)) -> 
    (* Make sure that x interval starts before y. If not, call the function 
       again with swapped arguments. *)
    if ax > ay then interval_sum y x
    (* If y starts before x finishes then result is simply single interval: *) 
    else if ay <= bx then Single (ax, max bx by)
    (* Otherwise interval MUST be of type double. It is not possible to get 
       different interval using modifiers implemented below. *)
    else 
      (assert (ax = neg_infinity && by = infinity) ; 
       Double (bx, ay))
  | (Double (ax, bx), Double (ay, by)) -> 
    double_interval (max ax ay) (min bx by)
  (* The result must be single, or double interval, hence 
     ay <= ax or by >= bx. *)
  | (Double (ax, bx), Single (ay, by)) -> 
    if ay <= ax then double_interval by bx
    else if by >= bx then double_interval ax ay
    else raise (Failure "Wrong context. Cannot produce more than 2 intervals.")
  (* Same case as above, with swapped arguments. *)
  | (Single (ax, bx), Double (ay, by)) -> interval_sum y x

let wartosc_dokladnosc x p =
  (* When x is negative, ends of interval are swapped. *)
  let a = x +. x *. p /. 100.
  and b = x -. x *. p /. 100.
  in Single (min a b, max a b)

let wartosc_od_do x y = 
  assert (x <= y);
  Single (x, y)

let wartosc_dokladna x =
  Single (x, x)

let in_wartosc w x =
  match w with
  | Empty -> false
  | Single (a, b) -> (x >= a && x <= b)
  | Double (a, b) -> (x <= a || x >= b)

let min_wartosc w =
  match w with
  | Empty -> nan
  | Single (a, _) -> a
  | Double (_, _) -> neg_infinity

let max_wartosc w =
  match w with
  | Empty -> nan
  | Single (_, b) -> b
  | Double (_, _) -> infinity

let sr_wartosc w =
  match w with
  | Single (a, b) -> 
    if a = neg_infinity && b = infinity then nan 
    else (a +. b) /. 2.
  | (_) -> nan

let rec plus x y =
  match (x, y) with
  (* The product is empty when one of the intervals is empty. *)
  | (Empty, _) -> Empty 
  | (_, Empty) -> Empty 
  | (Single (ax, bx), Single (ay, by)) -> 
    Single (ax +. ay, bx +. by)
  (* Any number can be produced when adding two double intervals: *)
  | (Double (_, _), Double (_, _)) -> 
    Single (neg_infinity, infinity)
  | (Double (ax, bx), Single (ay, by)) -> 
    double_interval (ax +. by) (bx +. ay)
  (* Symmetrical case: swap parameters and do the same as above. *)
  | (Single (_, _), Double (_, _)) -> 
    plus y x

let minus x y = 
  (* Subtraction is the addition of the opposite number. *)
  plus x (neg_interval y) 

let rec razy x y =
  match (x, y) with
  | (Empty, _) -> Empty
  | (_, Empty) -> Empty
  (* Calcuate largest and smallest possible value based on ends of 
     the intervals. The result of the multiplication of single 
     intervals is always single interval. *)
  | (Single (ax, bx), Single (ay, by)) -> 
    let inf = min (min (ax *. ay) (ax *. by)) (min (bx *. ay) (bx *. by)) 
    and sup = max (max (ax *. ay) (ax *. by)) (max (bx *. ay) (bx *. by)) 
    in Single (inf, sup)
  (* Multiplication of more intervals is sum of single 
     intervals multiplication *)
  | (Double (ax, bx), Double (ay, by)) -> 
    interval_sum 
      (interval_sum 
         (razy (Single (neg_infinity, ax)) (Single (neg_infinity, ay)))
         (razy (Single (neg_infinity, ax)) (Single (by, infinity))))
      (interval_sum 
         (razy (Single (bx, infinity)) (Single (neg_infinity, ay)))
         (razy (Single (bx, infinity)) (Single (by, infinity))))
  | (Double (ax, bx), Single (ay, by)) -> 
    interval_sum 
      (razy (Single (neg_infinity, ax)) (Single (ay, by)))
      (razy (Single (bx, infinity)) (Single (ay, by)))
  (* Symmetrical case: swap parameters and do the same as above. *)
  | (Single (_, _), Double (_, _)) -> 
    razy y x 

let rec podzielic x y =
  match (x, y) with
  | (Empty, _) -> Empty
  | (_, Empty) -> Empty
  (* The division by <0;0> case. It is the only way to produce empty set 
     from two none-empty sets. *)
  | (Single (_, _), Single (0., 0.)) -> Empty
  (* Division of single intervals: *)
  | (Single (ax, bx), Single (ay, by)) -> 
    (* When zero belongs to the interval but is not the end of it, the output 
       is sum of two divisions. NOTE: The output is always double interval.*)
    if ay < 0. && by > 0. then 
      interval_sum 
        (podzielic x (Single (ay, 0.)))
        (podzielic x (Single (0., by)))
        (* All numbers in second interval are >= 0 so it is possible to compute
            largest and smallest as it was a multiplication. *)
    else if ay >= 0. then 
      let inf = min (min (ax /. ay) (ax /. by)) (min (bx /. ay) (bx /. by))
      and sup = max (max (ax /. ay) (ax /. by)) (max (bx /. ay) (bx /. by))
      in Single (inf, sup)
      (* Tricky case: it is not possible to compute values as above, because
          when right side is > 0 it is possible to get infinity, although it is
          not in the result. Eg.: <4;8> / <-5;0>. So both values must be 
          multiplied by -1 and then computed in the case above. *)
    else 
      podzielic (neg_interval x) (neg_interval y)
  (* Any number can be produced when dividing two double intervals. *)
  | (Double (_, _), Double (_, _)) -> 
    Single (neg_infinity, infinity)
  (* Division of more intervals is sum of divisions of single intervals. *)
  | (Double (ax, bx), Single (_, _)) ->
    interval_sum 
      (podzielic (Single (neg_infinity, ax)) y)
      (podzielic (Single (bx, infinity)) y)
  (* Analogous case. *)
  | (Single (_, _), Double (ay, by)) -> 
    interval_sum 
      (podzielic x (Single (neg_infinity, ay)))
      (podzielic x (Single (by, infinity)))

(*
(* ======================= TESTS: ======================= *)
let neg_inf_to_zero = (* (-inf;0> *) 
  podzielic (wartosc_dokladnosc (-1.) 100.) (wartosc_od_do 0. 1.) 
let zero_to_inf = (* <0;inf) *)
  razy neg_inf_to_zero (wartosc_od_do (-9.) (-1.))
let real = plus (* (-inf;inf) *)
    (podzielic (wartosc_od_do 4. 9.) (wartosc_od_do (-3.) 2.))
    (podzielic (wartosc_od_do 200. 209.) (wartosc_od_do (-10.) 0.444))
let zero = (* <0;0> *)
  wartosc_dokladna 0.    
let empty_set = (* 'o *)
  podzielic (wartosc_dokladnosc 0.001 40.) zero
let test = (* (-inf; -2> U <2;+inf) *) 
  podzielic (wartosc_dokladna 2.) (wartosc_od_do (-1.) 1.)
let test0 = (* 'o *)
  podzielic zero_to_inf zero  
let test1 = (* (-inf;-4.000000> U <4.000000;inf) *)
  razy test test
let test2 = (* <0.000000;0.000000> *)
  razy test zero
let test3 = (* <0.000000;0.000000> *)
  razy (podzielic test (wartosc_dokladna 2.)) zero
let test4 = (* <0.000000;0.000000> *)
  razy real zero 
let test5 = (* <-63.000000;63.000000> *)
  razy (wartosc_od_do (-7.) 7.) (wartosc_od_do (-8.) 9.)
let test6 = (* <-2.000000;0.000000> *)
  razy (wartosc_dokladna (-1.)) (wartosc_od_do 0. (2.))
let test7 = (* <-inf;inf> *)
  razy neg_inf_to_zero (wartosc_od_do (-1.) 1.)
let test8 = (* <-inf;0.000000> *)
  razy zero_to_inf (wartosc_dokladnosc (-0.00000001) 100.)
let test9 = (* (-inf;-1.000000> U <1.000000;inf) *)
  podzielic (wartosc_dokladna (-1.)) (wartosc_od_do (-1.) 1.)
let test10 = (* 'o *)
  podzielic (wartosc_dokladnosc (-1.) 75.) zero
let bar = 
  plus (razy (wartosc_dokladna 12.5) (podzielic test (wartosc_dokladna 2.))) 
    (wartosc_dokladna 7.5)
let test11 = (* (-inf;1.000000> U <12.000000;inf) *)
  plus (plus (razy (wartosc_dokladna 12.5) 
  (podzielic test (wartosc_dokladna 2.)))(wartosc_dokladna 7.5)) 
  (wartosc_od_do (-8.) 6.)
let test12 = (* <-65.000000;63.000000> *)
  plus test5 test6
let test13 = (* <-inf;inf> *)
  plus test9 test11
let test14 = (* <-inf;inf> *)
  plus test9 test6
let test15 = (* (-inf;-3.000000> U <-1.000000;inf) *)
  plus (wartosc_dokladna (-2.)) test9
let test16 = (* <-63.000000;65.000000> *)
  minus test5 test6
let test17 = minus (* <-inf;inf> *)
    test9 test11
let test18 = (* <-inf;inf> *)
  minus test9 test6
let test19 = (* (-inf;-3.000000> U <-1.000000;inf) *)
  minus (wartosc_dokladna (-2.)) test9
let test20 = (* <0.000000;4.000000> *)
  razy test6 test6
let test21 = (* (-inf;-16.000000> U <16.000000;inf) *)
  razy test1 test1
let test22 = 
  razy test1 test9 (* (-inf;-4.000000> U <4.000000;inf) *)
let test23 = (* <-inf;inf> *)
  razy test9 test6
let test24 = (* <0;0> *)
  podzielic zero test5 
let test25 = (* 'o *)
  podzielic test1 zero 
;;
assert ((in_wartosc neg_inf_to_zero (-3.), in_wartosc neg_inf_to_zero 0.01) =
        (true, false));
assert ((in_wartosc zero 0., sr_wartosc zero, max_wartosc zero) =
        (true, 0., 0.));
assert ((compare (sr_wartosc test1) nan = 0, in_wartosc test1 (-3.)) = 
        (true, false));
assert ((min_wartosc test1, max_wartosc test1) = (neg_infinity, infinity));
assert ((in_wartosc test11 0., in_wartosc test11 1., in_wartosc test11 2.) = 
        (true, true, false));
assert ((in_wartosc test2 0., in_wartosc test3 0.00000000001) = (true, false)); 
assert ((sr_wartosc test4, in_wartosc test5 (-64.)) = (0., false));
assert (sr_wartosc test6 = (-1.));
assert (compare (sr_wartosc test7) nan = 0);
assert ((sr_wartosc test8 = neg_infinity, min_wartosc test8 = neg_infinity) =
        (true, true));
assert ((in_wartosc test9 0., in_wartosc test9 (-1.), in_wartosc test9 1.) =
        (false, true, true));
assert ((in_wartosc test10 0., in_wartosc (plus real test10) 0.) = 
        (false, false));
assert ((in_wartosc test11 0., in_wartosc test11 2.) = (true, false));
assert ((min_wartosc zero, sr_wartosc zero, max_wartosc zero) = (0.,0.,0.));
assert (compare (sr_wartosc test11) nan = 0);
assert (compare (min_wartosc empty_set) nan = 0);
assert ((min_wartosc test12, max_wartosc test12) = (-65., 63.));
assert ((min_wartosc test13 = neg_infinity, in_wartosc test13 0., 
         max_wartosc test13 = infinity) = (true, true, true));
assert ((min_wartosc test14 = neg_infinity, in_wartosc test14 0., 
         max_wartosc test14 = infinity) = (true, true, true));
assert ((in_wartosc test15 0., in_wartosc test15 (-1.), 
         in_wartosc test15 (-2.)) = (true, true, false));
assert ((min_wartosc test16, max_wartosc test16) = (-63., 65.));
assert ((min_wartosc test17, max_wartosc test17) = (neg_infinity, infinity));
assert ((min_wartosc test18, max_wartosc test18) = (neg_infinity, infinity));
assert ((in_wartosc test19 (-3.), in_wartosc test19 (-2.)) = (true, false));
assert ((min_wartosc test20, max_wartosc test20)  = (0., 4.));
assert ((in_wartosc test21 (-16.), in_wartosc test21 (16.), 
         in_wartosc test21 (-15.), in_wartosc test21 (15.)) = 
        (true, true, false, false));
assert ((in_wartosc test22 (-4.), in_wartosc test22 (4.)) = (true, true));
assert ((min_wartosc test23 = neg_infinity, in_wartosc test23 0., 
         max_wartosc test23 = infinity) = (true, true, true));
assert ((min_wartosc test24, sr_wartosc test24, max_wartosc test24) = 
        (0., 0., 0.));
assert ((compare (min_wartosc test25) nan = 0))
*)