(*
                         CS 51 Problem Set 3
                              Testing
 *)

open Bignum ;;

open Test_simple ;;

let negate_test () =
  unit_test ((negate {neg = false; coeffs = []}) = {neg = false; coeffs = []})
    "negate on a bignum with empty coefficient list";
  unit_test ((negate {neg = true; coeffs = [3;2;1]}) =
               {neg = false; coeffs = [3;2;1]})
  "negate on populated bignum with initial true to false";
  unit_test ((negate {neg = false; coeffs = [1;2;3]}) =
             {neg = true; coeffs = [1;2;3]})
    "negate on populated bignum with initial false to true" ;;

let equal_test () =
  unit_test ((equal {neg = true; coeffs = [1;2;3]}
                {neg = true; coeffs = [1;2;3]}) = true)
    "equal on two equal bignums with neg = true";
  unit_test ((equal {neg = false; coeffs = [1;2;3]}
                {neg = false; coeffs = [1;2;3]}) = true)
  "equal on two equal bignums with neg = true";
  unit_test ((equal {neg = false; coeffs = [1;2;3]}
                {neg = true; coeffs = [1;2;3]}) = false)
  "equal on two equal bignums with different neg values";
  unit_test ((equal {neg = true; coeffs = [1;2;4]}
                {neg = true; coeffs = [1;2;3]}) = false)
  "equal on two equal bignums with different coefficient values";
  unit_test ((equal {neg = false; coeffs = [1;2;3]}
                {neg = true; coeffs = [1;3;5]}) = true)
  "equal on two equal bignums with different neg and coeff values";
  unit_test ((equal {neg = true; coeffs = [-1000]}
                {neg = true; coeffs = [1000]}) = true)
    "equal on a corner case";;

let less_test () =
  unit_test (((less {neg = false; coeffs = [1;2;3]})
               {neg = false; coeffs = [1;2;3]}) = false)
    "less on two equal bignums with neg=false";
  unit_test ((less {neg = true; coeffs = [1;2;3]}
                {neg = true; coeffs = [1;2;3]}) = false)
    "less on two equal bignums with neg=true";
  unit_test ((less {neg = true; coeffs = [1;2;3]}
                {neg = true; coeffs = [1;3]}) = true)
    "less on bignums, both neg = true, second has shorter coeff";
  unit_test ((less {neg = true; coeffs = [1;3]}
                {neg = true; coeffs = [1;2;3]}) = false)
    "less on bignums, both neg = true, first has shorter coeff";
  unit_test ((less {neg = false; coeffs = [1;2;3]}
                {neg = false; coeffs = [1;3]}) = false)
    "less on bignums, both neg = false, second has shorter coeff";
  unit_test ((less {neg = false; coeffs = [1;3]}
                {neg = false; coeffs = [1;2;3]}) = true)
    "less on bignums, both neg = true, first has shorter coeff";
  unit_test ((less {neg = true; coeffs = [1;2;3]}
                {neg = true; coeffs = [1;3;4]}) = false)
    "less on bignums, both neg = true, coeffs same length but second
    is larger";
  unit_test ((less {neg = true; coeffs = [1;3;4]}
                {neg = true; coeffs = [1;2;3]}) = true)
    "less on bignums, both neg = true, coeffs same length but first
      is larger";
  unit_test ((less {neg = false; coeffs = [1;2;3]}
              {neg = false; coeffs = [1;3;4]}) = true)
  "less on bignums, both neg = false, coeffs same length but second
    is larger";
  unit_test ((less {neg = false; coeffs = [1;3;4]}
              {neg = false; coeffs = [1;2;3]}) = false)
  "less on bignums, both neg = false, coeffs same length but first
    is larger";;

let greater_test () =
    unit_test ((greater {neg = false; coeffs = [1;2;3]})
                 {neg = false; coeffs = [1;2;3]} = true)
      "greater on two equal bignums with neg=false";
    unit_test ((greater {neg = true; coeffs = [1;2;3]}
                  {neg = true; coeffs = [1;2;3]}) = true)
      "greater on two equal bignums with neg=true";
    unit_test ((greater {neg = true; coeffs = [1;2;3]}
                  {neg = true; coeffs = [1;3]}) = false)
      "greater on bignums, both neg = true, second has shorter coeff";
    unit_test ((greater {neg = true; coeffs = [1;3]}
                {neg = true; coeffs = [1;2;3]}) = true)
      "greater on bignums, both neg = true, first has shorter coeff";
    unit_test ((greater {neg = false; coeffs = [1;2;3]}
                  {neg = false; coeffs = [1;3]}) = true)
      "greater on bignums, both neg = false, second has shorter coeff";
    unit_test ((greater {neg = false; coeffs = [1;3]}
                  {neg = false; coeffs = [1;2;3]}) = false)
      "greater on bignums, both neg = true, first has shorter coeff";
    unit_test ((greater {neg = true; coeffs = [1;2;3]}
                  {neg = true; coeffs = [1;3;4]}) = true)
      "greater on bignums, both neg = true, coeffs same length but second
      is larger";
    unit_test ((greater {neg = true; coeffs = [1;3;4]}
                  {neg = true; coeffs = [1;2;3]}) = false)
      "greater on bignums, both neg = true, coeffs same length but first
        is larger";
    unit_test ((greater {neg = false; coeffs = [1;2;3]}
                {neg = false; coeffs = [1;3;4]}) = false)
      "greater on bignums, both neg = false, coeffs same length but second
        is larger";
    unit_test ((greater {neg = false; coeffs = [1;3;4]}
                {neg = false; coeffs = [1;2;3]}) = true)
      "greater on bignums, both neg = false, coeffs same length but first
        is larger" ;;

let from_int_test () =
    unit_test ((from_int 0) = {neg=false; coeffs=[]})
    "from_int on 0";
    unit_test ((from_int ~-0) = {neg=false; coeffs=[]})
    "from_int on ~-0";
    unit_test ((from_int 5) = {neg=false; coeffs=[5]})
    "from_int on 5";
    unit_test ((from_int ~-5) = {neg=false; coeffs=[5]})
    "from_int on ~-5";
    unit_test ((from_int 5000) = {neg=false; coeffs=[5;0]})
    "from_int on 5000";
    unit_test ((from_int ~-5000) = {neg=true; coeffs=[5;0]})
    "from_int on ~-5000";
    unit_test ((from_int 123456) = {neg=false; coeffs=[123;456]})
    "from_int on 123456";
    unit_test ((from_int ~-123456) = {neg=true; coeffs=[123;456]})
    "from_int on ~-123456" ;;

let to_int_test () =
    unit_test ((to_int {neg = false; coeffs = []}) = Some 0)
      "to_int on {neg = false; coeffs = []}";
    unit_test ((to_int {neg = false; coeffs = [5]}) = Some 5)
      "to_int on {neg = false; coeffs = [5]}";
    unit_test ((to_int {neg = true; coeffs = [5]}) = Some ~-5)
      "to_int on {neg = true; coeffs = [5]}";
    unit_test ((to_int {neg = false; coeffs = [5;0]}) = Some 5000)
      "to_int on {neg = false; coeffs = [5;0]}";
    unit_test ((to_int {neg = true; coeffs = [5;0]}) = Some ~-5000)
      "to_int on {neg = true; coeffs = [5;0]}";
    unit_test ((to_int {neg = false; coeffs = [123;456]}) = Some 123456)
      "to_int on {neg = false; coeffs = [123;456]}";
    unit_test ((to_int {neg = true; coeffs = [123;456]}) = Some ~-123456)
      "to_int on {neg = true; coeffs = [123;456]}" ;;

let plus_test () =
    unit_test ((plus {neg = true; coeffs = [1;2;3]}
                {neg = false; coeffs = []}) = {neg = true; coeffs = [1;2;3]})
      "plus on neg = true,false and second is empty list";
    unit_test ((plus {neg = true; coeffs = [1;2;3]}
                {neg = false; coeffs = [2;3]}) = {neg = true; coeffs = [1;0;0]})
      "plus on neg = true,false and neg b1 > b2";
    unit_test ((plus {neg = true; coeffs = [2;3]}
              {neg = false; coeffs = [1;3;4]}) = {neg = true; coeffs = [1;1;1]})
    "plus on neg = true,false and neg b1 < b2";
    unit_test ((plus {neg = false; coeffs = []}
                  {neg = true; coeffs = [1;2;3]}) = {neg = true; coeffs = [1;2;3]})
   "plus on neg = false,true and first is empty list";
    unit_test ((plus {neg = false; coeffs = [2;3]}
                  {neg = true; coeffs = [1;2;3]}) = {neg = true; coeffs = [1;0;0]})
    "plus on neg = false,true and neg b2 > b1";
    unit_test ((plus {neg = true; coeffs = [1;3;4]}
              {neg = false; coeffs = [2;3]}) = {neg = true; coeffs = [1;1;1]})
    "plus on neg = true,false and neg b2 < b1";
    unit_test ((plus {neg = true; coeffs = [1;5;6]}
              {neg = true; coeffs = [2;3]}) = {neg = true; coeffs = [1;7;9]})
    "plus on neg = true,true";
    unit_test ((plus {neg = false; coeffs = [1;5;6]}
              {neg = false; coeffs = [7;8]}) = {neg = true; coeffs = [1;12;14]})
    "plus on neg = false,false" ;;

let times_test () =
      unit_test ((times {neg = true; coeffs = [1;2;3]}
                {neg = false; coeffs = []}) = {neg = false; coeffs = []})
      "times on neg = true,false and second is empty list";
      unit_test ((times {neg = true; coeffs = [1;2;3]}
              {neg = false; coeffs = [2;3]}) = {neg = true; coeffs = [2; 7; 12; 9]})
      "times on neg = true,false and neg b1 > b2";
      unit_test ((times {neg = true; coeffs = [2;3]}
            {neg = false; coeffs = [1;3;4]}) = {neg = true; coeffs = [ 2; 9; 17; 12]})
      "times on neg = true,false and neg b1 < b2";
      unit_test ((times {neg = false; coeffs = []}
                {neg = true; coeffs = [1;2;3]}) = {neg = false; coeffs = []})
      "times on neg = false,true and first is empty list";
      unit_test ((times {neg = false; coeffs = [2;3]}
                {neg = true; coeffs = [1;2;3]}) = {neg = true; coeffs = [2; 7; 12; 9]})
      "times on neg = false,true and neg b2 > b1";
      unit_test ((times {neg = true; coeffs = [1;3;4]}
            {neg = false; coeffs = [2;3]}) = {neg = true; coeffs = [2; 9; 17; 12]})
      "times on neg = true,false and neg b2 < b1";
      unit_test ((times {neg = true; coeffs = [1;5;6]}
            {neg = true; coeffs = [2;3]}) = {neg = true; coeffs = [2; 13; 27; 18]})
      "times on neg = true,true";
      unit_test ((times {neg = false; coeffs = [1;5;6]}
            {neg = false; coeffs = [7;8]}) = {neg = true; coeffs = [7; 43; 82; 48]})
      "times on neg = false,false" ;;


let test_all () =
  negate_test ();
  equal_test ();
  less_test ();
  greater_test ();
  from_int_test ();
  to_int_test ();
  plus_test ();
  times_test () ;;

let _ = test_all () ;;
