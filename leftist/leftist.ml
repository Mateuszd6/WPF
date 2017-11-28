(* Author: Mateusz Dudziński (394171)
   Reviewer: Michał Sidor (394731)
   Zadanie 2: Drzewa Lewicowe WPF Sem. 2017Z *)

type 'a queue =
  | Null
  (* left subtree * root value * right subtree * null path length *)
  | Node of 'a queue * 'a * 'a queue * int

(* Creates empty queue. *)
let empty = Null

exception Empty

(* Get the Null path length of the given node or empty tree. 
   It is set to -1 for the empty tree to simplify the computation. *)
let get_npl = function
  | Null -> -1
  | Node (_, _, _, npl) -> npl

(* Join two leftist trees. Takes the smaller root and its left branch and sets
   it as a part of the result. The result's right branch becomes merged right 
   branch of the tree with a smaller root and the second tree. *)
let rec join l_queue r_queue = 
  match l_queue, r_queue with
  | Null, _ -> r_queue
  | _, Null -> l_queue
  | Node(l_l_queue, l_val, l_r_queue, _), Node(_, r_val, _, _) ->
    (* Swap the arguments so that the root of the left tree is smaller than 
       the right one. *)
    if l_val > r_val then join r_queue l_queue
    else 
      (* Compute right subtree recursively. *)
      let right_subtree = join l_r_queue r_queue 
      in
      (* Get NPLs of both trees... *)
      let right_subtree_npl = get_npl right_subtree 
      and left_subtree_npl = get_npl l_l_queue 
      in 
      (* ... and merge them based on the computed values. *)
      if left_subtree_npl < right_subtree_npl then 
        Node(right_subtree, l_val, l_l_queue, left_subtree_npl + 1)
      else
        Node(l_l_queue, l_val, right_subtree, right_subtree_npl + 1)

let add value queue = 
  (* Construct a single-node tree and merge it with a queue given.
     If the queue is empty, constructed node is joined with empty queue 
     and becomes the result which is correct value. *)
  let single_node_tree = Node(Null, value, Null, 0)
  in join queue single_node_tree  

(* True only when queue is Null. *)
let is_empty = function
  | Null -> true
  | _ -> false

(* Throws Empty when queue is Null. Otherwise returns the root value and
   merges its subtrees into one tree. *)
let delete_min = function
  | Null -> raise Empty
  | Node (l_queue, w, r_queue, _) -> w, join l_queue r_queue  
