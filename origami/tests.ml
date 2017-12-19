open Origami;;

(* Prosty test, zwykly prosotat... *)
let test1 = prostokat (0., 0.) (20., 20.) |> zloz (30., 0.) (30., 10.);;

assert (test1 (0., 0.) = 1);;
assert (test1 (-1., 0.) = 0);;
assert (test1 (1., 1.) = 1);;
assert (test1 (12.1345, 19.213) = 1);;
assert (test1 (20., 20.) = 1);;
assert (test1 (21., 0.) = 0);;
assert (test1 (30., 0.) = 0);;

(* Teraz prawie wszystkie odwrotnie bo 
   odwrotnie prosta wzgl. ktorej skladamy... *)
let test2 = prostokat (0., 0.) (20., 20.) |> zloz (30., 10.) (30., 0.);;

assert (test2 (0., 0.) = 1 |> not);;
assert (test2 (1., 1.) = 1 |> not);;
assert (test2 (12.1345, 19.213) = 1 |> not);;
assert (test2 (20., 20.) = 1 |> not);;
assert (test2 (21., 0.) = 0);;
assert (test2 (30., 0.) = 0);;

(* Skladamy prostokat pare razy... *)
let test3 = skladaj [((1., 0.), (1., 1.)) ; ((2., 0.), (2., 1.)) ; 
                     ((3., 0.), (3., 1.))] (prostokat (0., 0.) (5., 5.));;

assert(test3 (0., 0.) = 2);;
assert(test3 (1., 3.) = 1);;
assert(test3 (1.24, 0.5) = 0);;
assert(test3 (3.213, 2.2) = 0);;
assert(test3 (1., 1.) = 1);;
assert(test3 (5., 5.) = 0);;

let test4 = skladaj [((0., 9999999.), (0., -.1.)) ; ((-.100., 0.), (1231124., 0.))] 
    (kolko (0., 0.) 2.);;

assert (test4 (0., 0.) = 1);;
assert (test4 (0., 2.) = 2);;
assert (test4 (2., 0.) = 2);;
assert (test4 (1., 1.) = 4);;
assert (test4 (0.111123, 1.2) = 4);;
assert (test4 (-.0.111123, -.1.2) = 0);;

let test5 = skladaj [
    ((0., 1231.), (0., 0.));
    ((1., 627.), (2., 0.));
    ((2., 19961.), (4., 0.));
    ((3., 12567.), (8., 0.));
    ((4., 154351.), (16., 0.));
    ((5., 123453451.), (32., 0.));
    ((6., 653453451.), (64., 0.));
    ((7., 3453451.), (128., 0.));
    ((8., 12341.), (256., 0.)); 
    ((8., 3453451.), (512., 0.))] 
    (prostokat (0., 0.) (20147483648., 20147483648.));;

assert (test5 (-1., 0.) = 0);;
assert (test5 (0., 0.) = 0);;
assert (test5 (1., 1.) = 0);;
assert (test5 (517., 517.) = 2);;

let test6 = skladaj [
    ((100., 100.), (1., 1.));
    ((0., 100.), (0., 0.1))]
    (kolko (0., 0.) 2.);;

assert (test6 (0.2, -0.2) = 4);;
assert (test6 (-0.2, -0.2) = 0);;
assert (test6 (1., 0.) = 2);;
assert (test6 (-0.000000001, 0.) = 0);;


print_string "Testing completed. Have a good day :)\n";;
