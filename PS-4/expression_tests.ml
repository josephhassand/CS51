(*
                         CS 51 Problem Set 4
                 A Language for Symbolic Mathematics
                               Testing
 *)

open Expression ;;
open ExpressionLibrary ;;

open Absbook ;;

let contains_var_test () =
    unit_test (contains_var (parse "x+3")) "contains_var sum left";
    unit_test (contains_var (parse "3+x")) "contains_var sum right";
    unit_test (contains_var (parse "x-3")) "contains_var subtraction left";
    unit_test (contains_var (parse "3-x")) "contains_var subtraction right";
    unit_test (contains_var (parse "x*3")) "contains_var multiplication left";
    unit_test (contains_var (parse "3*x")) "contains_var multiplication right";
    unit_test (contains_var (parse "x/3")) "contains_var division left";
    unit_test (contains_var (parse "3/x")) "contains_var division right";
    unit_test (contains_var (parse "x^3")) "contains_var power left";
    unit_test (contains_var (parse "3^x")) "contains_var power right";
    unit_test (contains_var (parse "sin x")) "contains_var sin x";
    unit_test (contains_var (parse "cos x")) "contains_var cos x";
    unit_test (contains_var (parse "ln x")) "contains_var ln x";
    unit_test (contains_var (parse "~x")) "contains_var neg x";
    unit_test (not (contains_var (parse "2"))) "contains_var only number";
    unit_test (not (contains_var (parse "1+3"))) "contains_var sum only number";
    unit_test (not (contains_var (parse "1-3"))) "contains_var subtraction only number";
    unit_test (not (contains_var (parse "1*3"))) "contains_var multiplication only number";
    unit_test (not (contains_var (parse "1/3"))) "contains_var division only number";
    unit_test (not (contains_var (parse "3^3"))) "contains_var power only number";
    unit_test (not (contains_var (parse "sin 3"))) "contains_var sin only number";
    unit_test (not (contains_var (parse "cos 3"))) "contains_var cos only number";
    unit_test (not (contains_var (parse "ln 3"))) "contains_var only number";
    unit_test (not (contains_var (parse "~3"))) "contains_var neg only number";;


let evaluate_test () =
  unit_test ((evaluate (parse "x+1") 2.) = 3.) "evaluate on binop addition";
  unit_test ((evaluate (parse "x-1") 2.) = 1.) "evaluate on binop subtraction";
  unit_test ((evaluate (parse "x*1") 2.) = 2.) "evaluate on binop multiplication";
  unit_test ((evaluate (parse "x/2") 4.) = 2.) "evaluate on binop division";
  unit_test ((evaluate (parse "x^2") 2.) = 4.) "evaluate on binop power";
  unit_test ((evaluate (parse "sin x") 0.) = 0.) "evaluate on unop sin";
  unit_test ((evaluate (parse "cos x") 0.) = 1.) "evaluate on unop cos";
  unit_test ((evaluate (parse "ln x") 1.) = 0.) "evaluate on unop ln";
  unit_test ((evaluate (parse "~x") 1.) = Float.neg(1.)) "evaluate on unop neg";
  unit_test ((evaluate (parse "3+4") 3.) = 7.) "evaluate on expression with no variables";
  unit_test ((evaluate (parse "sin (x^2)") 2.) = sin 4.) "evaluate on unop and binop
                                                            operation";;


let derivative_test () =
  unit_test (to_string (derivative (parse "2")) = "0.") "derivative on an
      expression without a variable";
  unit_test (to_string (derivative (parse "x")) = "1.") "derivative on x";
  unit_test (to_string (derivative (parse "x^2")) = "(2.*((x^2.)/x))") "derivative
      on x^2";
  unit_test (to_string (derivative (parse "x+x")) = "(1.+1.)")
    "derivative on binop addition";
  unit_test (to_string (derivative (parse "x-x")) = "(1.-1.)")
    "derivative on binop subtraction";
  unit_test (to_string (derivative (parse "x*x")) = "((x*1.)+(1.*x))") "derivative
on binop multiplication";
  unit_test (to_string (derivative (parse "x/x")) = "(((x*1.)-(1.*x))/(x*x))")
    "derivative on binop division";
  unit_test (to_string (derivative (parse "x^x")) = "((x*((x^x)/x))*1.)")
    "derivative on binop power";
  unit_test (to_string (derivative (parse "sin x")) = "((cos(x))*1.)")
    "derivative on unop sin";
  unit_test (to_string (derivative (parse "cos x")) = "((~((sin(x))))*1.)")
    "derivative on unop cos";
  unit_test (to_string (derivative (parse "ln x")) = "((1./x)*1.)")
    "derivative on unop ln";
  unit_test (to_string (derivative (parse "~x")) = "(~(1.))")
    "derivative on unop neg";;


let find_zero_test () =
  unit_test ((find_zero (parse "x") 3. 0.001 5) = Some 0.) "find_zero on x";
  unit_test ((find_zero (parse "3") 3. 0.001 5) = None) "find_zero on 3, no variable";
  unit_test ((find_zero (parse "(x*4)-12") 3. 0.001 5) = Some 3.) "find_zero on (x*4)-12";;


let test_all () =
  contains_var_test();
  evaluate_test () ;
  derivative_test ();
  find_zero_test ();;

let _ = test_all () ;;
