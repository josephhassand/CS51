(*
                         CS 51 Problem Set 2
            Higher Order Functional Programming -- Testing
 *)

open Mapfold ;;

open Test_simple ;;      (* a really simple unit testing framework *)

let negate_all_test () =
  unit_test ((negate_all []) = [])
            "negate_all empty";
  unit_test ((negate_all [1; -2; 0]) = [-1; 2; 0])
    "negate_all mixed";;

let sum_test () =
  unit_test ((sum[]) = 0)
        "sum on an empty list";
  unit_test ((sum [1; 2; 3]) = 6)
    "sum a list with positive integers";
  unit_test ((sum[-1; -2; -3]) = -6)
    "sum with only negative values";
  unit_test ((sum[1; -2; 3]) = 2)
    "sum with mixed negative and positive values";;

let sum_rows_test () =
  unit_test ((sum_rows[]) = [])
    "sum_rows with an empty list" ;
  unit_test ((sum_rows[[1;2];[];[]]) = [3; 0; 0])
  "sum_rows with a populated list" ;
  unit_test ((sum_rows[[1;2];[3;4];[]]) = [3; 7; 0])
    "sum_rows with two populated lists" ;
  unit_test ((sum_rows[[1;2;1];[2;2;2];[3;3;3]]) = [4;6;9])
      "sum_rows with three populated list" ;;

let filter_odd_test () =
  unit_test ((filter_odd[]) = [])
    "filter_odd with an empty list";
  unit_test ((filter_odd[1;2;3;4]) = [1;3])
    "filter_odd with a list composed of positive numbers";
  unit_test ((filter_odd[1;2;3;-4;-5]) = [1;3;-5])
    "filter_odd with a list composed of positive and negative numbers";;

  let num_occurs_test () =
  unit_test ((num_occurs 1 []) = 0)
      "num_occurs with an empty list";
  unit_test ((num_occurs 1 [1;2;1;2;1]) = 3)
    "num_list with various values including the target value";
  unit_test ((num_occurs 3 [1;2;1;2;1]) = 0)
    "num_list with a list without the target value";
  unit_test ((num_occurs 3 [3;3;3]) = 3)
    "num_list where the only value is the target value";;

  let super_sum_test () =
    unit_test ((super_sum []) = 0)
      "super_sum on an empty list";
    unit_test ((super_sum [[1;2];[3;4];[]]) = 10)
      "super_sum on lists of which one is empty";
    unit_test ((super_sum [[1;2];[3;4];[5;6]]) = 21)
      "super_sum on three full lists";
    unit_test ((super_sum [[1;2];[3;4];[-1;-2]]) = 7)
      "super_sum on lists with negative values";;

let filter_range_test () =
  unit_test ((filter_range[](0,0)) = [])
    "filter_range with an empty list and no range";
  unit_test ((filter_range[1;2;3;4;5](1,3)) = [1;2;3])
    "filter_range with a full list and applicable range";
  unit_test ((filter_range[1;2;3;4;5](3,5)) = [3;4;5])
    "filter_range with a full list and a different applicable range";
  unit_test ((filter_range[1;2;3;4;5](3,1)) = [])
    "filter_range with an inapplicable range";
  unit_test ((filter_range[1;3;5;2;4](1,3)) = [1;3;2])
    "filter_range with an unordered list";
  unit_test ((filter_range[1;-2;3;-4;5](-1,3)) = [1;3])
    "filter_range with negative values in list and range";;

let floats_of_ints_test () =
  unit_test ((floats_of_ints []) = [])
    "floats_of_ints for an empty list";
  unit_test ((floats_of_ints [0;1;2;3]) = [0.;1.;2.;3.])
    "floats_of_ints for a full list, including 0";
  unit_test ((floats_of_ints [-1]) = [-1.])
    "floats_of_ints for a list with a negative value";;

let log10s_test () =
  unit_test ((log10s []) = [None])
    "log10s with an empty list";
  unit_test ((log10s [1.0; 10.0; -10.0]) = [Some 0.; Some 1.; None])
    "log10s with a full list";;


let deoptionalize_test () =
  unit_test ((deoptionalize[]) = [])
    "doptionalize on an empty list";
  unit_test ((deoptionalize[None]) = [])
    "doptionalize on a list with solely the value None";
  unit_test ((deoptionalize[Some 2; None; Some 4; Some 10]) = [2;4;10])
    "doptionalize on a list with values";;

let some_sum_test () =
  unit_test ((some_sum[]) = 0)
    "some_sum on an empty list";
  unit_test ((some_sum[None]) = 0)
    "some_sum on a list with only the None element";
  unit_test ((some_sum[Some 1]) = 1)
    "some_sum on a list with a single Some element";
  unit_test ((some_sum[Some 1; Some 2; None; Some 3; Some 4]) = 10)
    "some_sum on a longer list";;

let mult_odds_test () =
  unit_test ((mult_odds[]) = 1)
    "mult_odds on an empty list";
  unit_test ((mult_odds [3]) = 3)
    "mult_odds on a list containing a single odd element";
  unit_test ((mult_odds[2;4;6]) = 1)
    "mult_odds on a list solely composed of even integers";
  unit_test ((mult_odds[1;2;3;0;-1;-2;-3]) = -9)
    "mult_odds on a longer list, including negative numbers and zeroes";;

let concat_test () =
  unit_test ((concat[[];[];[]]) = [])
   "concat on a set of empty lists";
  unit_test ((concat[[1;2;3];[4];[5;6;7;8]]) = [1;2;3;4;5;6;7;8])
    "concat on a longer set of integer values";
  unit_test ((concat[["My"; "name"];["is"];["Joseph"]]) = ["My; name is Joseph"])
    "concat a set of lists that are strings";
  unit_test ((concat[[false ; true];[false;true]]) = [false;true;false;true])
    "concat a set of lists that are bools";;

let filter_by_year_test () =
  unit_test((filter_by_year [] 2000) = [])
    "filter_by_year with an empty list";
  unit_test((filter_by_year [("John", 2001);("Angela",2002)] 2001) = ["John"])
    "filter_by_year with two values, one corresponding to the target year";
  unit_test((filter_by_year [("Dwight",1999);("Michael",1998);("Jim",1999)] 1999)
            = ["Dwight";"Jim"])
    "filter_by_year with three values, two of which correspond to the target
value";;


let test_all () =
    negate_all_test ();
    sum_test ();
    sum_rows_test ();
    filter_odd_test ();
    num_occurs_test ();
    super_sum_test ();
    filter_range_test ();
    floats_of_ints_test ();
    log10s_test ();
    deoptionalize_test ();
    some_sum_test ();
    mult_odds_test ();
    concat_test ();
    filter_by_year_test ();;

let _ = test_all () ;;
