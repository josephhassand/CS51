(*
                              CS51 Lab 4
               Error Handling, Options, and Exceptions
 *)


(*======================================================================
Part 1: Currying and uncurrying

Before getting into the main topic of this lab, how to handle
anomalous conditions using option types and exceptions, we continue
with some exercises about polymorphism.

........................................................................
Exercise 1: In the next two exercises, you'll define polymorphic
higher-order function `curry` and `uncurry` for currying and uncurrying
binary functions (functions of two arguments). The functions are named
after mathematician Haskell Curry '1920. (By way of reminder, a
curried function takes its arguments one at a time. An uncurried
function takes them all at once in a tuple.)

We start with the polymorphic higher-order function `curry`, which takes
as its argument an uncurried function and returns the curried version
of its argument function.

Before starting to code, pull out a sheet of paper and a pencil and
with your partner work out the answers to the following seven
questions:

1. What is the type of the argument to the function `curry`? Write down
   a type expression for the argument type.

2. What is an example of a function that `curry` could apply to?

3. What is the type of the result of the function `curry`? Write down a
   type expression for the result type.

4. What should the result of applying the function `curry` to the
   function from (2) be?

5. Given (1) and (2), write down a type expression for the type of the
   `curry` function itself.

6. What would a good variable name for the argument to `curry` be?

7. Write down the header line for the definition of the `curry` function.

Call over a staff member to go over your answers to these
questions. Once you fully understand all this, its time to implement
the function `curry`.
......................................................................*)

let curry (uncurried : ('a * 'b) -> 'c) (x : 'a) (y : 'b) : 'c =
  uncurried(x,y) ;;

(*
   let curry uncurried x y = uncurried (x, y) ;;
   Here, we see what's really going on: `curry uncurried` when applied
   to `x` and `y` in curried fashion gives the same value that `uncurried`
   gives when applied to `x` and `y` in uncurried fashion. *)

(*......................................................................
Exercise 2: Now implement the polymorphic higher-order function
`uncurry`, which takes as its argument a curried function and returns
the uncurried version of its argument function.  You may want to go
through the same 7-step process to get started.
......................................................................*)

let uncurry (curried: 'a -> 'b -> 'c) ((x,y) : ('a  * 'b)) : 'c =
  curried x y;;

(* Here saying that function uncurry curried receiving inputs all together is
   the same as a curried function receiving inputs separately. *)

(*......................................................................
Exercise 3: OCaml's built in binary operators, like ( + ) and ( * ) are
curried. You can tell from their types:

    # ( + ) ;;
    - : int -> int -> int = <fun>
    # ( * ) ;;
    - : int -> int -> int = <fun>

Using your `uncurry` function, define uncurried versions of the plus and
times functions.
......................................................................*)

let plus =
  uncurry ( + ) ;;

let times =
  uncurry ( * ) ;;

(*......................................................................
Exercise 4: Recall the `prods` function from Lab 1:

    let rec prods (lst : (int * int) list) : int list =
      match lst with
      | [] -> []
      | (x, y) :: tail -> (x * y) :: (prods tail) ;;

Now reimplement `prods` using `map` and your uncurried `times`
function. Why do you need the uncurried `times` function?
......................................................................*)

let prods =
  List.map times ;;

(* Needs to be the uncurried times function as the function needs to accept
   two values (the next one from the list and the one currently had)
   simultaneously in order to complete its operation. *)

(*======================================================================
Part 2: Option types

In Lab 2, you implemented a function `max_list` that returns the maximum
element in a non-empty integer list. Here's a possible implementation
for `max_list`:

    let rec max_list (lst : int list) : int =
      match lst with
      | [elt] -> elt
      | head :: tail -> max head (max_list tail) ;;

(This implementation makes use of the polymorphic `max` function from
the Stdlib module.)

As written, this function generates a warning that the match is not
exhaustive. Why? What's an example of the missing case? Try entering
the function in ocaml and see what information you can glean from the
warning message.


The problem is that there is no reasonable value for the maximum
element in an empty list. This is an ideal application for option
types.

........................................................................
Exercise 5:

Reimplement `max_list`, but this time, it should return an `int option`
instead of an `int`. Call it `max_list_opt`. The `None` return value
should be used when called on an empty list.
......................................................................*)

let rec  max_list_opt (lst : int list) : int option =
  match lst with
  | [] -> None
  | hd :: tl ->
    match (max_list_opt tl) with
    | None -> Some hd
    | Some max_tail -> Some (max hd max_tail) ;;

(*......................................................................
Exercise 6: Alternatively, we could have `max_list` raise an exception
upon discovering the error condition. Reimplement `max_list` so that it
does so. What exception should it raise? (See Section 10.2 in the
textbook for some advice.)
......................................................................*)

let rec max_list (lst : int list) : int =
  match lst with
  | [] -> raise (Invalid_argument "user inputted an empty list")
  | [elt] -> elt
  | head :: tail -> max head (max_list tail) ;;

(*......................................................................
Exercise 7: Write a function `min_option` to return the smaller of its
two `int option` arguments, or `None` if both are `None`. If exactly one
argument is `None`, return the other. The built-in function `min` from
the Stdlib module may be useful. You'll want to make sure that all
possible cases are handled; no nonexhaustive match warnings!
......................................................................*)

let min_option (x : int option) (y : int option) : int option =
  match x,y with
  | None, None -> None
  | Some a , None ->  x
  | None, Some b -> y
  | Some a , Some b -> Some (min a b) ;;

(*......................................................................
Exercise 8: Write a function `plus_option` to return the sum of its two
`int option` arguments, or `None` if both are `None`. If exactly one
argument is `None`, return the other.
......................................................................*)

let plus_option (x : int option) (y : int option) : int option =
match x,y with
| None, None -> None
| Some a , None ->  x
| None, Some b -> y
| Some a , Some b -> Some (a + b) ;;


(*======================================================================
Part 3: Polymorphism practice

........................................................................
Exercise 9: Do you see a pattern in your implementations of `min_option`
and `plus_option`? How can we factor out similar code?

Write a polymorphic higher-order function `calc_option` to extend
binary operations to operate on option type values, taking three
arguments in order: the binary operation (a curried function) and its
first and second arguments as option types. If both arguments are
`None`, return `None`.  If one argument is `None`, the function should
return the other argument. If neither argument is `None`, the binary
operation should be applied to the argument values and the result
appropriately returned.

What is the type signature for `calc_option`? (If you're having
trouble figuring that out, call over a staff member, or check our
intended type at https://url.cs51.io/lab4-1.)

Now implement `calc_option`.
......................................................................*)

let calc_option (f: 'a -> 'a -> 'a) (x: 'a option) (y: 'a option) :
  'a option=
  match x , y with
  | None, None -> None
  | Some a , None ->  x
  | None, Some b -> y
  | Some a , Some b -> Some (f a b) ;;

(*......................................................................
Exercise 10: Now rewrite `min_option` and `plus_option` using the
higher-order function `calc_option`. Call them `min_option_2` and
`plus_option_2`.
......................................................................*)

let min_option_2 x y =
  calc_option (min) x y ;;

let plus_option_2 x y =
  calc_option (+) x y ;;

(*......................................................................
Exercise 11: Now that we have `calc_option`, we can use it in other
ways. Because `calc_option` is polymorphic, it can work on things other
than `int option`s. Define a function `and_option` to return the boolean
AND of two `bool option`s, or `None` if both are `None`. If exactly one
is `None`, return the other.
......................................................................*)

let and_option : bool option -> bool option -> bool option =
  calc_option (&&) ;;

(*......................................................................
Exercise 12: In Lab 3, you implemented a polymorphic function `zip` that
takes two lists and "zips" them together into a list of pairs. Here's
a possible implementation of zip:

    let rec zip (x : 'a list) (y : 'b list) : ('a * 'b) list =
      match x, y with
      | [], [] -> []
      | xhd :: xtl, yhd :: ytl -> (xhd, yhd) :: (zip xtl ytl) ;;

A problem with the implementation of `zip` is that, once again, its
match is not exhaustive and it raises an exception when given lists of
unequal length. How can you use option types to generate an alternate
solution without this property?

Do so below in a new definition of `zip`, called `zip_opt` to make clear
that its signature has changed, which returns an appropriate option
type in case it is called with lists of unequal length.
......................................................................*)

let rec zip_opt (x : 'a list) (y : 'b list) : (('a * 'b) list) option =
  match (x,y) with
  | [], [] -> Some []
  | xhd :: xtl, yhd :: ytl ->
      (match zip_opt xtl ytl with
      | None -> None
      | Some ztl -> Some ((xhd, yhd) :: ztl))
  | _, _ -> None
;;

(*====================================================================
Part 4: Factoring out None-handling

Recall the definition of `dotprod` from Lab 2. Here it is adjusted to
an option type:

    let dotprod (a : int list) (b : int list) : int option =
      let pairsopt = zip_opt a b in
      match pairsopt with
      | None -> None
      | Some pairs -> Some (sum (prods pairs)) ;;

It uses `zip_opt` from Exercise 12 and `prods` from Exercise 4. The sum
function is simply *)

let sum : int list -> int =
  List.fold_left (+) 0 ;;

(* Notice how in dotprod and other option-manipulating functions we
frequently and annoyingly have to test if a value of option type is
`None`, requiring a separate match, and passing on the `None` value in the
"bad" branch or introducing the `Some` in the "good" branch. This is
something we're likely to be doing a lot of. Let's factor that out to
simplify the implementation.

........................................................................
Exercise 13: Define a function called `maybe` that takes a function of
type `'a -> 'b` and an argument of type `'a option`, and "maybe"
(depending on whether its argument is a `None` or a `Some`) applies the
function to the argument. The `maybe` function either passes on the
`None` if its first argument is `None`, or if its first argument is
`Some v`, it applies its second argument to that `v` and returns the
result, appropriately adjusted for the result type.

What should the type of the `maybe` function be?

Now implement the `maybe` function.
......................................................................*)

let maybe (f : 'a -> 'b) (x : 'a option) : 'b option =
  match x with
  | None -> None
  | Some v -> Some (f v)  ;;

(*......................................................................
Exercise 14: Now reimplement `dotprod` to use the `maybe` function. (The
previous implementation makes use of functions `sum` and `prods`. You've
already (re)implemented `prods` in Exercise 4. We've provided `sum` for
you above.)  Your new solution for `dotprod` should be much simpler than
the version we provided above at the top of Part 4.
......................................................................*)

let dotprod (a : int list) (b : int list) : int option =
  maybe (fun pairs -> sum (prods pairs)) (zip_opt a b);;

(*......................................................................
Exercise 15: Reimplement `zip_opt` along the same lines, as `zip_opt_2`
below.
......................................................................*)

let rec zip_opt_2 (x : 'a list) (y : 'b list) : (('a * 'b) list) option =
  match (x,y) with
  | [], [] -> Some []
  | xhd :: xtl, yhd :: ytl -> maybe (fun ztl -> (xhd, yhd) :: ztl)(zip_opt_2 xtl ytl)
  | _,_ -> None ;;

(*......................................................................
Exercise 16: For the energetic, reimplement `max_list_opt` along the
same lines. There's likely to be a subtle issue here, since the `maybe`
function always passes along the `None`.
......................................................................*)

let rec max_list_opt_2 (lst : int list) : int option =
  match lst with
  | [] -> None
  | [single] -> Some single
  | hd :: tl -> maybe (fun max_tail -> max hd max_tail)(max_list_opt_2 tl)
;;
