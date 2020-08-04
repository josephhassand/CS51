(*
                              CS51 Lab 12
               Imperative Programming and References
 *)

(*
Objective:

This lab provides practice with reference types and their use in
building mutable data structures and in imperative programming more
generally. It also gives further practice in using modules to abstract
data types.
 *)

(*====================================================================
Part 1: Fun with references

......................................................................
Exercise 1: Consider a function `incr` that takes an `int ref`
argument and has the side effect of incrementing the integer stored in
the `ref`. What is an appropriate type for the return value? What
should the type for the function as a whole be?
....................................................................*)
(* int ref -> unit *)

(* Know the answer? Call over a staff member to check. If you're
working on this lab outside of lab times, check
<https://url.cs51.io/lab12-1> for our answer, which you should keep to
in the next exercise. *)
(*....................................................................
Exercise 2: Now implement the `incr` function. (As usual, for this and
succeeding exercises, you shouldn't feel beholden to how the
definition is introduced in the skeleton code below. For instance, you
might want to add a `rec`, or use a different argument list, or no
argument list at all but binding to an anonymous function instead.)
....................................................................*)

let incr (n: int ref) : unit =
  n := !n + 1 ;;

(*....................................................................
Exercise 3: Write a function `remember` that returns the last string
that it was called with. The first time it is called, it should return
the empty string.

    # remember "don't forget" ;;
    - : string = ""
    # remember "what was that again?" ;;
    - : string = "don't forget"
    # remember "etaoin shrdlu" ;;
    - : string = "what was that again?"

(This is probably the least functional function ever written!)

As usual, you shouldn't feel beholden to how the definition is
introduced in the skeleton code below.
....................................................................*)

let remember: string -> string =
  let memory = ref "" in
  fun (input: string) ->
    let previous = !memory in
    memory := input;
    previous ;;


(*====================================================================
Part 2: Gensym

The `gensym` function (short for "GENerate SYMbol") has a long history
dating back to the early days of the programming language LISP.  You
can find it as early as the 1974 MacLISP manual (page 53).
http://www.softwarepreservation.org/projects/LISP/MIT/Moon-MACLISP_Reference_Manual-Apr_08_1974.pdf

(What is LISP you ask? LISP is an untyped functional programming
language invented by John McCarthy in 1958, which he based directly on
Alonzo Church's lambda calculus. It is one of the most influential
programming languages ever devised. You could do worse than spend some
time learning the Scheme dialect of LISP, which, by the way, will be
made much easier by having learned a typed functional language --
OCaml.)

The `gensym` function takes a string and generates a new string by
suffixing a unique number, which is initially 0 but is incremented
each time gensym is called.

For example,

    # gensym "x" ;;
    - : string = "x0"
    # gensym "x" ;;
    - : string = "x1"
    # gensym "something" ;;
    - : string = "something2"
    # gensym (gensym "x") ;;
    - : string = "x34"

There are many uses of `gensym`, one of which is to generate new
unique variable names in an interpreter for a programming language. In
fact, you'll need `gensym` for just this purpose in completing the
final project for CS51, so this is definitely not wasted effort.

......................................................................
Exercise 4: Complete the implementation of `gensym`. As usual, you
shouldn't feel beholden to how the definition is introduced in the
skeleton code below. (We'll stop mentioning this from now on.)
....................................................................*)

let gensym: string -> string =
  let x = ref (-1) in
  fun (s: string) ->
    incr x;
    s ^ string_of_int (!x) ;;

(*====================================================================
Part 3: Appending mutable lists

Recall the definition of the mutable list type from Section 15.4 of
the textbook: *)

type 'a mlist =
  | Nil
  | Cons of 'a * ('a mlist ref) ;;

(* Mutable lists are just like regular lists, except that the tail of
each "cons" is a *reference* to a mutable list, so that it can be
updated. *)

(*....................................................................
Exercise 5: Define a polymorphic function `mlist_of_list` that
converts a regular list to a mutable list, with behavior like this:

    # let xs = mlist_of_list ["a"; "b"; "c"];;
    val xs : string mlist =
      Cons ("a",
            {contents = Cons ("b",
                              {contents = Cons ("c",
                                                {contents = Nil})})})

    # let ys = mlist_of_list [1; 2; 3];;
    val ys : int mlist =
      Cons (1,
            {contents = Cons (2,
                              {contents = Cons (3,
                                                {contents = Nil})})})
....................................................................*)

let rec mlist_of_list (lst : 'a list) : 'a mlist =
  match lst with
  | [] -> Nil
  | hd :: tl -> Cons(hd, {contents = mlist_of_list tl}) ;;

(*....................................................................
Exercise 6: Define a function `length` to compute the length of an
`mlist`. Try to do this without looking at the solution that is given
in the book. (Don't worry about cycles...yet.)

    # length Nil ;;
    - : int = 0
    # length (mlist_of_list [1;2;3;4]) ;;
    - : int = 4
....................................................................*)

let rec length (mlst : 'a mlist) : int =
  match mlst with
  | Nil -> 0
  | Cons(hd, tl) -> 1 + length !tl
;;

(*....................................................................
Exercise 7: What is the time complexity of the `length` function in
O() notation in terms of the length of its list argument?
....................................................................*)
(* It is time complexity O(n), or in other words it is linear.*)

(*....................................................................
Exercise 8: Now, define a function `mappend` that takes a *non-empty*
mutable list and a second mutable list and, as a side effect, causes
the first to become the appending of the two lists. Some questions to
think about before you get started:

 o What is an appropriate return type for the `mappend` function? (You
   can glean our intended answer from the examples below, but try to
   think it through yourself first.

 o Why is there a restriction that the first list be non-empty?

 o What is the appropriate thing to do if `mappend` is called with an
   empty mutable list as first argument?

Examples of use:

    # let m1 = mlist_of_list [1; 2; 3];;
    val m1 : int mlist =
      Cons (1, {contents = Cons (2, {contents = Cons (3, {contents = Nil})})})

    # let m2 = mlist_of_list [4; 5; 6];;
    val m2 : int mlist =
      Cons (4, {contents = Cons (5, {contents = Cons (6, {contents = Nil})})})

    # length m1;;
    - : int = 3

    # mappend m1 m2;;
    - : unit = ()

    # length m1;;
    - : int = 6

    # m1;;
    - : int mlist =
    Cons (1,
     {contents =
       Cons (2,
        {contents =
          Cons (3,
           {contents =
             Cons (4,
              {contents = Cons (5, {contents = Cons (6, {contents = Nil})})})})})})
....................................................................*)

let rec mappend (mlst1: 'a mlist) (mlst2: 'a mlist) : unit =
  match mlst1 with
  | Nil -> ()
  | Cons (_hd, tl) ->
    match !tl with
    | Nil -> tl := mlst2
    | Cons (_, _) as x -> mappend x mlst2 ;;

(* What happens when you evaluate the following expressions
   sequentially in order?

      # let m = mlist_of_list [1; 2; 3] ;;
      # mappend m m ;;
      # m ;;
      # length m ;;

   Do you understand what's going on? *)

(*====================================================================
Part 4: Adding serialization to imperative queues

In the textbook, we defined a polymorphic imperative queue signature
as follows:

    module type IMP_QUEUE =
      sig
        type 'a queue
        val empty : unit -> 'a queue
        val enq : 'a -> 'a queue -> unit
        val deq : 'a queue -> 'a option
      end

In this part, you'll add functionality to imperative queues for
serializing of the queue. (You added serialization to a pure
(immutable) stack module in Lab 8.) The signature needs to be
augmented first; we've done that for you here: *)

module type IMP_QUEUE =
  sig
    type elt
    type queue

    val empty : unit -> queue
    val enq : elt -> queue -> unit
    val deq : queue -> elt option
    val to_string : queue -> string
  end ;;

(* Note that we've changed the module slightly so that the element
type (`elt`) is made explicit.

The `to_string` function needs to know how to convert the individual
elements to strings. That information is best communicated by
packaging up the element type and its own `to_string` function in a
module that can serve as the argument to a functor for making
`IMP_QUEUE`s. (You'll recall this technique from Lab 8.)

Given the ability to convert the elements to strings, the `to_string`
function should work by converting each element to a string in order
separated by arrows " -> " and with a final end marker to mark the end
of the queue "||". For instance, the queue generated by enqueing, in
order, integer elements 1, 2, and 3 would serialize to the string
"1 -> 2 -> 3 -> ||" (as shown in the example below).

We've provided a functor for making imperative queues, which works
almost identically to the final implementation from the book (based on
mutable lists) except for abstracting out the element type in the
functor argument.

......................................................................
Exercise 9: Your job is to complete the implementation by finishing
the `to_string` function. (Read on below for an example of the use of
the functor.)
....................................................................*)

module MakeImpQueue (A : sig
                           type t
                           val to_string : t -> string
                         end)
                  : (IMP_QUEUE with type elt = A.t) =
  struct
    type elt = A.t
    type mlist = Nil | Cons of elt * (mlist ref)
    type queue = {front: mlist ref ; rear: mlist ref}

    let empty () = {front = ref Nil; rear = ref Nil}
    let enq x q =
      match !(q.rear) with
      | Cons (_h, t) -> assert (!t = Nil);
                        t := Cons(x, ref Nil);
                        q.rear := !t
      | Nil -> assert (!(q.front) = Nil);
               q.front := Cons(x, ref Nil);
               q.rear := !(q.front)
    let deq q =
      match !(q.front) with
      | Cons (h, t) ->
         q.front := !t ;
         (match !t with
          | Nil -> q.rear := Nil
          | Cons(_, _) -> ());
         Some h
      | Nil -> None
    let to_string q =
      let rec to_string' mlst =
        match !mlst with
          | Nil -> "||"
          | Cons(hd, tl) -> Printf.sprintf "%s -> %s" (A.to_string hd) (to_string' tl)
      in to_string' q.front
  end ;;

(* To build an imperative queue, we apply the functor to an
appropriate argument. For instance, we can make an integer queue
module: *)

module IntQueue = MakeImpQueue (struct
                                  type t = int
                                  let to_string = string_of_int
                                end) ;;

(* And now we can test it by enqueueing some elements and converting
the queue to a string to make sure that the right elements are in
there. *)

let test () =
  let open IntQueue in
  let q = empty () in
  enq 1 q;
  enq 2 q;
  enq 3 q;
  to_string q ;;

(* Running the `test` function should have the following behavior:

    # test () ;;
    - : string = "1 -> 2 -> 3 -> ||"

*)
