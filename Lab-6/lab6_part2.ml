(*
                              CS51 Lab 6
     Variants, algebraic types, and pattern matching (continued)
 *)

(* Objective: This lab is intended to reinforce core concepts in
   typing in OCaml, including:

     Algebraic data types
     Using algebraic data types to enforce invariants
     Implementing polymorphic algebraic data types
 *)

(*======================================================================
                         Part 2: Binary trees

Binary trees are a data structure composed of nodes that store a value
from some base type (say, `int`) as well as a left and a right
subtree. To well-found this recursive definition, a binary tree can
also be a leaf. For purpose of this lab, we'll say that the leaves
store no further data, though many definitions of binary trees do
store data at the leaves.

Defined in this way, trees resemble lists, except they have two
"tails" rather than one.

........................................................................
Exercise 7: Define a polymorphic binary tree data type called
`bintree`. Then take a look at the definition that we were expecting at
<https://url.cs51.io/lab6-2> and make sure that your definition is
consistent with it so that your further work in the lab will be
consistent with our unit tests.
......................................................................*)

type 'a bintree =
  | Leaf
  | Node of 'a * 'a bintree * 'a bintree ;;

(*......................................................................
Exercise 8: Define a function `leaf_count : 'a bintree -> int`, which
returns the number of leaves in a binary tree.
......................................................................*)

let rec leaf_count (tree : 'a bintree) : int =
  match tree with
    | Leaf -> 1
    | Node (a, b, c) -> leaf_count b + leaf_count c ;;

(*......................................................................
Exercise 9: Define a function `find`, such that `find tree value`
returns `true` if `value` is stored at some node in `tree` and `false`
otherwise.
......................................................................*)
let rec find (tree : 'a bintree) (value : 'a) : bool =
  match tree with
  | Leaf -> false
  | Node (stored, left, right) -> stored =
                                  value || find left value || find right value;;

(*......................................................................
Exercise 10: Define a function `min_value`, such that `min_value tree`
returns the minimum value stored in `tree` as an option type, and
`None` if the tree has no stored values. For purposes of determining
the minimum, use the `<` operator for comparing values stored in the
nodes of the tree.
......................................................................*)

let rec min_value (tree : 'a bintree): 'a option =
  let min_opt x y =
    match x, y with
    | None, None -> None
    | None, Some v -> Some v
    | Some v, None -> Some v
    | Some v1, Some v2 -> Some (if v1 < v2 then v1 else v2) in
  match tree with
  | Leaf -> None
  | Node (value, left, right) -> min_opt (Some value) (min_opt (min_value left) (min_value right)) ;;

(*......................................................................
Exercise 11: Define a function `map_tree`, such that `map_tree fn
tree` returns a tree that is structured just like its argument tree
but applying the function `fn` to each of the values in the
tree. You'll want to think carefully about the type of `map_tree` to
maximize its polymorphism.
......................................................................*)

let rec map_tree (fn : 'a -> 'b) (tree : 'a bintree) : 'b bintree =
  match tree with
  | Leaf -> Leaf
  | Node (value, left, right) -> Node (fn value, map_tree fn left, map_tree fn right) ;;
