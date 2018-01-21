open Przelewanka;;

assert (przelewanka [| |] = 0);;

assert (przelewanka [|(2000, 3)|] = -1);;

assert (przelewanka [|(2000, 2000)|] = 1);;

assert (przelewanka [|(0, 0)|] = 0);;

assert (przelewanka [|(0, 0); (10, 0)|] = 0);;

assert (przelewanka [|(0, 0); (2, 2)|] = 1);;

assert (przelewanka [|(0, 0); (10, 0); (20, 10); (0, 0)|] = 2);;

assert (przelewanka [|(2,1); (4,3); (8,7); (16,15); (32,31)|] = -1);;

assert (przelewanka [|(1, 1); (20, 10)|] = 20);;

assert (przelewanka [|(1, 1); (10, 9); (20, 19)|] = 5);;

assert (przelewanka [|(3, 1); (4, 2); (6, 5); (123, 12); 
  (10, 2); (12, 2) |] = -1);;

assert (przelewanka [|(3, 1); (4, 2); (10, 0) |] = 8);;

assert (przelewanka [|(1, 0); (10000, 5000)|] = 10000);;
 
assert (przelewanka [|(2, 1); (3, 2); (4, 3); (5, 4); (6, 0)|] = 8);

assert (przelewanka [|(0, 0); (0, 0); (0, 0); (0, 0); (0, 0); 
  (0, 0); (2, 1); (3, 2); (4, 3); (5, 4); (6, 0)|] = 8);

assert (przelewanka [|(1, 0); (1000000, 999996)|] = 9);;

assert (przelewanka [|(1, 0); (5, 3); (6, 1); (10, 10)|] = 6);;
assert (przelewanka [| (21, 11); (22, 12)|] = -1);;

assert (przelewanka [| (21, 11); (22, 0)|] = 41);;

assert (przelewanka [| (1000, 2); (2000, 3); (3000, 4); (1, 0)|] = 18);;

assert (przelewanka [| (10, 0); (20, 0); (30, 0); (40, 0); (50, 0); (60, 0); 
  (70, 0); (80, 0); (90, 20) |] = 2);;

assert (przelewanka [|(10, 2); (3, 2); (4,1 ); (5, 4); (6, 0)|] = 9);;

assert (przelewanka [|(10, 2); (3, 2); (4,1 ); (5, 4)|] = -1);;

assert (przelewanka [|(5, 4); (4, 3); (3, 2); (2, 1); (1, 0)|] = 7);;

assert (przelewanka [|(2,0); (2,2); (2,1); (6,6); (0,0) |] = (-1));;

assert (przelewanka [|(5, 0); (4, 0); (3, 0)|] = 0);;

assert (przelewanka [|(5, 4); (4, 0); (3, 2)|] = 5);;

assert (przelewanka [|(1, 1); (2, 1); (3, 1)|] = 3);;

assert (przelewanka [|(5, 4); (4, 0); (3, 2)|] = 5);;

assert (przelewanka [|(10000, 10000); (9999, 9999); (9998, 9998)|] = 3);;

assert (przelewanka [|(0, 0); (4, 2); (2, 0)|] = 2);;

assert (przelewanka [|(0, 0); (4, 0); (999, 11); (2, 0)|] = 495);;

assert (przelewanka [|(12, 11); (11, 10); (10, 9); (9, 8); (2,0)|] = 11);;

assert (przelewanka [|(1, 1)|] = 1);;

assert (przelewanka [|(10000, 9999); (1, 1)|] = 2);;

(* To juz calkeim spory test jak na brute-force'a. *)
assert (przelewanka [|(8, 7); (7, 6); (6, 5); (5, 4); 
  (4, 3); (3, 2); (2, 1); (1, 0)|] = 13);;

print_string "Testing completed!\n";;
