(*
			                   CS 51 Problem Set 4
                 A Language for Symbolic Mathematics
 *)

(*======================================================================
Before reading this code (or in tandem), read the problem set 4
writeup. It provides context and crucial information for completing
the problems.

We provide a type definition for arithmetic expressions over floating
point numbers with a single variable. The type definition can be found
at the top of expressionLibrary.ml, along with enumerated type
definitions for the unary and binary operators, and other useful
functions. You will be using this algebraic data type for this part of
the problem set.

The module ExpressionLibrary is opened here to provide you with access
to the expression data type and helpful functions that you will use
for this part of the problem set.
......................................................................*)

 open ExpressionLibrary ;;

(*......................................................................
Tips:

1. READ THE WRITEUP, particularly for the definition of the derivative
   function.

2. Use the type definitions provided at the top of
   expressionLibrary.ml as a reference, and don't change any of the
   code in that file. It provides functions such as "parse" and
   "to_string_smart" that will be helpful in this problem set.
......................................................................*)

  (*......................................................................
Problem 1: The function `contains_var` tests whether an expression
contains a variable `x`. For example:

# contains_var (parse "x^4") ;;
- : bool = true
# contains_var (parse "4+3") ;;
- : bool = false
......................................................................*)

let rec contains_var (e : expression) : bool =
  match e with
  | Num _  -> false
  | Var -> true
  | Binop (_, firstexp , secondexp) -> contains_var(firstexp) || contains_var(secondexp)
  | Unop (_ , firstexp) -> contains_var(firstexp) ;;

(*......................................................................
Problem 2: The function `evaluate` evaluates an expression for a
particular value of `x`. Don't worry about specially handling the
"divide by zero" case. For example:

# evaluate (parse "x^4 + 3") 2.0
- : float = 19.0
......................................................................*)

let rec evaluate (e : expression) (x : float) : float =
  match e with
  | Num value -> value
  | Var -> x
  | Binop (binopexp, firstexp, secondexp)->
    (match binopexp with
    | Add -> (evaluate firstexp x) +. (evaluate secondexp x)
    | Sub -> (evaluate firstexp x) -. (evaluate secondexp x)
    | Mul -> (evaluate firstexp x) *. (evaluate secondexp x)
    | Div -> (evaluate firstexp x) /. (evaluate secondexp x)
    | Pow -> (evaluate firstexp x) ** (evaluate secondexp x))
  | Unop (unopexp, firstexp) ->
    (match unopexp with
      | Sin -> sin(evaluate firstexp x)
      | Cos -> cos(evaluate firstexp x)
      | Ln -> log(evaluate firstexp x)
      | Neg -> (-1.) *. (evaluate firstexp x)) ;;

(*......................................................................
Problem 3: The `derivative` function returns the expression that
represents the derivative of the argument expression. We provide the
skeleton of the implementation here along with a few of the cases;
you're responsible for filling in the remaining parts that implement
the derivative transformation provided in the figure in the
writeup. See the writeup for instructions.
......................................................................*)

let rec derivative (e : expression) : expression =
  match e with
  | Num _ -> Num 0.
  | Var -> Num 1.
  | Unop (u, e1) ->
     (match u with
      | Sin -> Binop (Mul, Unop (Cos, e1), derivative e1)
      | Cos -> Binop (Mul, Unop (Neg, Unop (Sin, e1)), derivative e1)
      | Ln -> Binop(Mul, (Binop (Div, derivative e1, e1)), derivative e1)
      | Neg -> Unop (Neg, derivative e1))
  | Binop (b, e1, e2) ->
     match b with
     | Add -> Binop (Add, derivative e1, derivative e2)
     | Sub -> Binop (Sub, derivative e1, derivative e2)
     | Mul -> Binop (Add, Binop (Mul, e1, derivative e2),
                     Binop (Mul, derivative e1, e2))
     | Div -> Binop(Div, (Binop (Sub, Binop (Mul, e1, derivative e2),
                                 Binop (Mul, derivative e1, e2))),
                    Binop(Mul, e1, e1))
     | Pow ->
        (* split based on whether the exponent has any variables *)
        if contains_var e2
        then Binop(Mul, (Binop(Mul, e2, Binop(Div, Binop(Pow, e1, e2), e1))),
                   derivative e2)
        else Binop(Mul, e2, (Binop(Div, Binop(Pow, e1, e2), e1))) ;;

(* A helpful function for testing. See the writeup. *)
let checkexp strs xval =
  print_string ("Checking expression: " ^ strs ^ "\n");
  let parsed = parse strs in
  (print_string "contains variable : ";
   print_string (string_of_bool (contains_var parsed));
   print_endline " ";
   print_string "Result of evaluation: ";
   print_float (evaluate parsed xval);
   print_endline " ";
   print_string "Result of derivative: ";
   print_endline " ";
   print_string (to_string (derivative parsed));
   print_endline " ") ;;

(*......................................................................
Problem 4: Zero-finding. See writeup for instructions.
......................................................................*)

let new_guess (input: float) (expr: expression) : float =
  input -. (evaluate expr input /. evaluate (derivative expr) input) ;;


let rec find_zero (expr : expression)
              (guess : float)
              (epsilon : float)
              (limit : int)
  : float option =
    match limit with
    | 0 -> None
    | _ -> if (abs_float (evaluate expr guess) <= epsilon) then Some guess else
      let newg = new_guess guess expr
      in find_zero expr newg epsilon (limit - 1) ;;

find_zero (parse "(x*2)-8") 3. 0.001 5;;


(*......................................................................
Problem 5: Challenge problem -- exact zero-finding. This problem is
not counted for credit and is not required. Just leave it
unimplemented if you do not want to do it. See writeup for
instructions.
......................................................................*)

let find_zero_exact (e : expression) : expression option =
  failwith "find_zero_exact not implemented" ;;

(*======================================================================
Reflection on the problem set

After each problem set, we'll ask you to reflect on your experience.
We care about your responses and will use them to help guide us in
creating and improving future assignments.

........................................................................
Please give us an honest (if approximate) estimate of how long (in
minutes) this problem set took you to complete.
......................................................................*)

let minutes_spent_on_pset () : int =
  180 ;;

(*......................................................................
It's worth reflecting on the work you did on this problem set, where
you ran into problems and how you ended up resolving them. What might
you have done in retrospect that would have allowed you to generate as
good a submission in less time? Please provide us your thoughts in the
string below.
......................................................................*)

let reflection () : string =
  "I enjoyed working on this pset. Particularly, I liked how the functions
evaluate and derivative were reused in the find_zero function, and made its completion
pretty easy and straightforward." ;;
