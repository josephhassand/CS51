(*
                         CS 51 Problem Set 1
                     Core Functional Programming
 *)

(*======================================================================
Part 1 - Practice with simple functions

We start with writing a few simple functions as a warm up. Some of these
may also be useful in Part 2 as well.

For each subproblem, you will implement a given function, and provide
appropriate unit tests in the accompanying file pset1_tests.ml. You are
provided a high level description as well as a type signature of the
function you must implement. Keep in mind the CS51 style guide and what
you've learned so far about code quality and elegance. You should *not*
use library functions (for instance, those in the `List` module) for
*this* problem.

We recommend that you specify your unit tests for a function *before*
working on writing the function. This development cycle of first
understanding the function requirements, creating unit tests, then
implementing the function is highly recommended for all future work in
CS51 and hopefully beyond.  Following such a development cycle will
give you a clearer idea of what it is you'll be implementing, and
helps improve your understanding of the task at hand before getting
deep into the code, and will hopefully minimize bugs and
headaches. Unit tests should be your first activity, not an
afterthought.

As an example, we've already provided some unit tests in
pset1_tests.ml for the `nonincreasing` function. (Of course, you should
feel free to add more.)

IMPORTANT NOTE: In these problem sets and in the labs as well, we'll
often supply some skeleton code that looks like this:

    let somefunction (arg1 : type) (arg2 : type) : returntype =
      failwith "somefunction not implemented"

We provide this to give you an idea of the function's intended name
and its signature (its type, including its arguments and their types,
and the return type). But there's no need to slavishly follow that
particular way of implementing code to those specifications. In
particular, you may want to modify the first line to introduce, say, a
rec keyword (if your function is to be recursive):

    let rec somefunction (arg1 : type) (arg2 : type) : returntype =
      ...your further code here...

Or you might want to define the function using anonymous function
syntax:

    let somefunction =
      fun (arg1 : type) (arg2 : type) : returntype ->
        ...your further code here...

In short, you can and should write these function definitions in as
idiomatic a form as possible, regardless of how the stub code has been
organized. *)

(*......................................................................
Problem 1a: Write a function `nonincreasing` that takes a list of
integers and returns `true` if and only if the list is in nonincreasing
order. The empty list is considered to be reversed in this
sense. Consecutive elements of the same value are allowed in a reversed
list.

For example:

    # nonincreasing [1; 2; 3] ;;
    - : bool = false
    # nonincreasing [1; 2; 1; 2] ;;
    - : bool = false
    # nonincreasing [3; 2; 1] ;;
    - : bool = true
    # nonincreasing [5; 2; 2; 2; 1; 1] ;;
    - : bool = true

Here is its signature:

    nonincreasing : int list -> bool

Start by writing a full set of unit tests in ps1_tests.ml. Try to
cover both typical cases and "edge cases". Once you have a good set of
unit tests, replace the line below with your own definition of
`nonincreasing`. (Recall the "important note" above.) Then compile and
run the tests to check that your function works, before moving on to
the next problem.
......................................................................*)

let rec nonincreasing (l: int list) : bool =
  match l with
  | [] -> false
  | hd :: [] -> false
  | hd :: a :: tl ->  if a < hd then true else (nonincreasing(tl));;

(*......................................................................
Problem 1b: Write a function `merge` that takes two integer lists, each
*sorted* in nondecreasing order, and returns a single merged list in
sorted order.  For example:

    # merge [1; 3; 5] [2; 4; 6] ;;
    - : int list = [1; 2; 3; 4; 5; 6]
    # merge [1; 2; 5] [2; 4; 6] ;;
    - : int list = [1; 2; 2; 4; 5; 6]
    # merge [1; 3; 5] [2; 4; 6; 12] ;;
    - : int list = [1; 2; 3; 4; 5; 6; 12]
    # merge [1; 3; 5; 700; 702] [2; 4; 6; 12] ;;
    - : int list = [1; 2; 3; 4; 5; 6; 12; 700; 702]

Here is its type signature:

    merge : int list -> int list -> int list

As before, you should first provide unit tests (but we've done that
for you for this problem), and then work on writing the
function. Replace the line below with your own definition of `merge`.
......................................................................*)

let rec nonincreasing (l: int list) : bool =
  match l with
  | [] -> false
  | hd :: [] -> false
  | hd :: a :: tl ->  if a < hd then true else (nonincreasing(tl));;

let rec orderlist (x : int list) : (int list) =
  match x with
  | [] -> []
  | hd :: tl -> insert hd (orderlist tl)
    and insert elt x =
    match x with
     | [] -> [elt]
     | hd :: tl -> if elt <= hd then elt :: x else hd :: insert elt tl;;


let merge (x : int list) (y : int list) : (int list) =
  match x, y with
  | [], [] -> []
  | xhd :: xtl, [] -> if nonincreasing(x) then orderlist(x) else x
  | [], yhd :: ytl -> if nonincreasing(y) then orderlist(y) else y
  | x , y -> orderlist(x @ y);;

(*......................................................................
Problem 1c: The function `unzip`, given a list of boolean pairs, returns
a pair of lists, the first of which contains each first element of each
pair, and the second of which contains each second element. The
returned list should have elements in the order in which they were
provided. For example:

    # unzip [(true, false); (false, false); (true, false)] ;;
    - : bool list * bool list = ([true; false; true], [false; false; false])

Here is its signature:

    unzip : (bool * bool) list -> bool list * bool list)

As before, you should first provide unit tests, and then work on
writing the function. (We'll stop reminding you about writing the unit
tests first, not because it's not important but because it ought to go
without saying.) Replace the line below with your own definition of
`unzip`.
......................................................................*)

let rec unzip (x: (bool * bool) list) : bool list * bool list =
  match x with
  | [] -> [] , []
  | (xhd, yhd) :: tl ->
    let xtl, ytl = unzip tl in (xhd::xtl, yhd::ytl)
;;

(*......................................................................
Problem 1d: One way to compress a list of characters is to use
run-length encoding. The basic idea is that whenever we have repeated
characters in a list such as

  ['a'; 'a'; 'a'; 'a'; 'a'; 'b'; 'b'; 'b'; 'a'; 'd'; 'd'; 'd'; 'd']

we can represent the same information more compactly (usually) as a
list of pairs like

  [(5, 'a'); (3, 'b'); (1, 'a'); (4, 'd')]      .

Here, the numbers represent how many times the character is
repeated. For example, the first character in the string is 'a' and it
is repeated 5 times in a row, followed by 3 occurrences of the
character 'b', followed by one more 'a', and finally 4 copies of 'd'.

Write a function `to_run_length` that converts a list of characters
into the run-length encoding and a function `from_run_length` that
converts back. Writing both functions will make it easier to test that
you've gotten them right.

Here are their signatures:

  to_run_length : char list -> (int * char) list
  from_run_length : (int * char) list -> char list

Replace the lines below with your own definitions of `to_run_length`
and `from_run_length`.
......................................................................*)

let rec charactercounter (lisst : char list) (number : int) (intermlist : (int * char) list) =
  let rec connectlists desired item =
    match desired with
      | [] -> [item]
      | hd :: tl -> hd :: connectlists tl item in
    match lisst with
      | [] -> []
      | [h] -> connectlists intermlist (number, h)
      | hda :: hdb :: tl ->
        if hda = hdb then charactercounter (hda :: tl) (number + 1) intermlist
        else charactercounter (hdb :: tl) 1 (connectlists intermlist (number, hda))

let to_run_length (lst : char list) : (int * char) list =
  match lst with
  | [] -> []
  | [h] -> [(1, h)]
  | _ :: _ -> charactercounter lst 1 [] ;;

let rec includelist reps letter =
    match reps with
    | 1 -> letter :: []
    | hd -> letter :: includelist (hd - 1) letter

let rec appender = function
  | [], tl -> tl
  | (hd :: t), tl -> hd :: appender(t, tl)

let rec from_run_length (originallist : (int * char) list) : char list =
  match originallist with
    | [] -> []
    | [(repetitions, ltr)] -> includelist repetitions ltr
    | (repetitionss, ltrr) :: tl ->
      appender( includelist repetitionss ltrr, from_run_length tl)
(*======================================================================
Part 2 - Prisoner's Dilemma

In the remainder of this problem set, you will be implementing a version
of the iterated prisoner's dilemma in OCaml.

We represent an action as a boolean.  The  boolean value `true` represents
a cooperation action, and the boolean value `false` represents a defection
action, which we codify in some constant definitions.

We represent a `play`, that is, one round of the prisoner's dilemma, as
a boolean tuple, where the first element represents your action and
the second element represents the other player's action. *)

type action = bool ;;

let cCOOPERATE = true ;;
let cDEFECT = false ;;

type play = action * action ;;

(* A payoff matrix will be represented as an association list, a list of
key-value pairs. the first entry of the tuple is a `play`, and the
second entry of the tuple is an `int * int` tuple, representing the
payoff to each player.  The first element of the payoff is your payoff,
and the second element is the other player's payoff.

We will represent a history of actions using a play list.  The head of
the list is the most recent round's actions. *)

type payoff_matrix = (play * (int * int)) list

(* Note: Do *not* modify this matrix. *)

let test_payoff_matrix : payoff_matrix =
  [ ( (cCOOPERATE, cCOOPERATE), (3, 3)  );
    ( (cCOOPERATE, cDEFECT),    (-2, 5) );
    ( (cDEFECT,    cCOOPERATE), (5, -2) );
    ( (cDEFECT,    cDEFECT),    (0, 0)  ) ] ;;

(*......................................................................
Problem 2a: Write a function `extract_entry` that given a `play` and a
`payoff_matrix` as input, returns the payoff for the given play. If
the play is not found in the payoff matrix, return a default tuple
value of (404, 404). (We'll introduce much better ways of signaling
error conditions later, as described in Chapter 10.)

Here is its signature:

    extract_entry : play -> payoff_matrix -> int * int

Replace the line below with your own definition of `extract_entry`.
......................................................................*)

let rec extract_entry (move: play) (matrix: payoff_matrix) : int * int =
  match matrix with
  | [] -> (404,404)
  | hd :: t -> let a,b = hd in
    if move = a then b else extract_entry move t;;

(* We will represent a history of actions using a `play list`. The
head of the list is the most recent round's actions.

We also represent a strategy type as a function from a history to a
action, that is, of type `history -> action`.

As an example, we define two basic strategies, `nasty` and `patsy`,
which will ignore their inputs and always defect or cooperate,
respectively. *)

type history = play list
type strategy = history -> action

let nasty : strategy =
  fun _ -> cDEFECT ;;

let patsy : strategy =
  fun _ -> cCOOPERATE ;;

(*......................................................................
Problem 2b: The above strategies are not very sophisticated. Let us
start working our way up to defining a more complex strategy. To do so,
we will need to define two helper functions, `count_defections` and
`count_cooperations`. These functions will take as input a history, and
return a tuple containing the number of defections or cooperations that
you and the other player made, respectively.

Here are its signatures:

  count_defections : history -> int * int
  count_cooperations : history -> int * int

Replace the lines below with your own definitions of `count_defections`
and `count_cooperations`. Try your best to reduce code duplication!
......................................................................*)

let rec counter (wanted_action : bool)(perscount, othercount : int * int)
    (history: history) : int * int =
  match history with
  | [] -> (perscount, othercount)
  | (persaction, otheraction) :: tl ->
      let rec incrementer count action =
        if action = wanted_action then count + 1 else count
      in counter wanted_action (incrementer perscount persaction,
                                incrementer othercount otheraction) tl ;;

let rec count_defections =
  counter cDEFECT (0,0) ;;

let count_cooperations =
  counter cCOOPERATE (0,0) ;;

(*......................................................................
Problem 2c: Define a balanced strategy. This strategy cooperates on the
first round, and then does the opposite action of its previous round's
action for every subsequent round. Recall that a `strategy` is actually a
function of type `history -> action`.
......................................................................*)

let balanced (hist : history) : action =
 match hist with
  | [] -> cCOOPERATE
  | (perslast, _) :: _ -> if perslast then cDEFECT else cCOOPERATE ;;

(*......................................................................
Problem 2d: Define an egalitarian strategy. This strategy cooperates
on the first round. On all subsequent rounds, the egalitarian strategy
examines the history of the other player's actions, counting the total
number of defections of each player. If the other player's defections
outnumber our defections, the strategy will defect; otherwise it will
cooperate.
......................................................................*)

let egalitarian (history : history) : action =
  match history with
  | [] -> cCOOPERATE
  | _ -> let (persdefecs, otherdefecs) = count_defections history in
    if persdefecs < otherdefecs then cDEFECT else cCOOPERATE ;;
(*......................................................................
Problem 2e: Define a tit-for-tat strategy. This strategy cooperates on
the first round, and then on every subsequent round it mimics the other
player's previous action.
......................................................................*)

let tit_for_tat (history : history) : action =
  match history with
  | [] -> cCOOPERATE
  | ( _ , otherlast):: _ -> otherlast ;;

(*......................................................................
Problem 2f: Now define your own strategy. Any strategy that compiles
and is not the `failwith` stub will receive full credit. If you'd
like, you can run this strategy in a round-robin tournament as
described in the problem set writeup. You may assume that the
tournament will use the payoff matrix defined by the original
`test_payoff_matrix`.

For this problem only, you may make use of the `Random` module if you
would like to. See its documentation online at
https://caml.inria.fr/pub/docs/manual-ocaml/libref/Random.html.

If you want to enter your strategy in the tournament, give it a
pseudonym as well. To maintain grading anonymity, please don't use
your real name -- choose an appropriate pseudonym.
......................................................................*)

let my_strategy (history : history) : action =
  match history with
  | [] -> cCOOPERATE
  | __ -> let (__, otherdefecs) =
            count_defections history in
            let (__, othercoops) = count_cooperations history in
            if othercoops > otherdefecs || othercoops = otherdefecs then cCOOPERATE
            else cDEFECT ;;

(* If you want to enter your strategy in the tournament, give it a
pseudonym here. *)
let my_pseudonym = "" ;;

(* In order to run a full iterated prisoner's dilemma, we need a few
more auxiliary functions. You'll write these now. *)

(*......................................................................
Problem 2g: Write a function `swap_actions` that given a history
returns a history where each tuple is swapped, though the order of
rounds in the history should still be preserved. This function will be
necessary for running two strategies against each other later in this
problem set. Here is its signature:

    swap_actions : history -> history

For example:

    swap_actions [(cDEFECT, cCOOPERATE); (cCOOPERATE, cCOOPERATE)]
                 = [(cCOOPERATE, cDEFECT); (cCOOPERATE, cCOOPERATE)]
......................................................................*)

let rec swap_actions (history : history) : history =
  match history with
  | [] -> []
  | (a , b) :: t -> (b , a) :: swap_actions t ;;
(*......................................................................
Problem 2h: Write a function `calculate_payoff` that given a
`payoff_matrix` and a history returns the total payoffs to you and
the other player generated by the given history. Here is its
signature:

    calculate_payoff : payoff_matrix -> history -> int * int
......................................................................*)

let rec payoff (perspayofftot , otherpayofftot : int * int)
    (matrix : payoff_matrix)
    (history : history) : int * int =
  match history with
  | [] -> perspayofftot , otherpayofftot
  | x :: tl -> let (perspayoff, otherpayoff) = extract_entry x matrix
    in payoff (perspayoff + perspayoff, otherpayoff + otherpayoff) matrix tl ;;

let calculate_payoff =
  payoff (0,0) ;;

(* All the parts are now in place to run an iterated prisoners
dilemma. We've provided a function below to do just that. Notice that
it makes good use of your `calculate_payoff` and `swap_actions`
functions.

    play_strategies n payoff_matrix strat1 strat2 -- Plays strategies
    `strat1` and `strat2` against each other for `n` rounds, returning
    the cumulative payoffs for both strategies based on the provided
    payoff_matrix`. *)

let play_strategies (n : int)
                    (payoff_matrix : payoff_matrix)
                    (first_strat : strategy)
                    (second_strat : strategy)
                  : int * int =

  (* extend_history init_history n -- Returns a history that starts with
     `init_history` and extends it by `n` more plays. *)
  let rec extend_history (init : history) (count : int) : history =
    if count = 0 then init
    else
      let first_action, second_action =
        first_strat init, second_strat (swap_actions init)
      in
      let new_history = (first_action, second_action) :: init in
      extend_history new_history (count - 1)
  in
  calculate_payoff payoff_matrix (extend_history [] n) ;;

(* Now we can test it out. We'll play Nasty versus Patsy for 100 rounds
and print out the result. To see this, uncomment the single line below
and then compile the file by running `make ps1` in your shell,
followed by the command `./ps1.byte`. Feel free to try out other
strategies by changing `first_strategy` and `second_strategy`
below. But make sure to recomment that last line before submitting
your problem set for grading. *)

let cROUNDS = 100 ;;
let first_strategy = nasty ;;
let second_strategy = patsy ;;

let main () =
  let first_payoff, second_payoff =
    play_strategies cROUNDS test_payoff_matrix first_strategy second_strategy
  in
  Printf.printf "first payoff: %d, second payoff: %d\n"
                first_payoff second_payoff ;;

(* Uncomment this when finished
let _ = main () ;;
 *)

(*======================================================================
Reflection on the problem set

After each problem set, we'll ask you to reflect on your experience.
We care about your responses and will use them to help guide us in
creating and improving future assignments.

........................................................................
Please give us an honest (if approximate) estimate of how long (in
minutes) this problem set took you to complete.
......................................................................*)

let minutes_spent_on_pset () : int = 900 ;;

(*......................................................................
It's worth reflecting on the work you did on this problem set, where
you ran into problems and how you ended up resolving them. What might
you have done in retrospect that would have allowed you to generate as
good a submission in less time? Please provide us your thoughts in the
string below.
......................................................................*)

let reflection () : string =
  "Overall, I found this problem set to be quite difficult, particularly
  given that my work with OCaml is minimal." ;;
