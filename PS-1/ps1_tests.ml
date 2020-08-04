(*
                         CS 51 Problem Set 1
                Core Functional Programming -- Testing
*)

open Ps1 ;;

(* unit_test test msg -- Returns unit, with side effect of printing a
   report identified by msg on whether the unit test passed (returned
   true) or failed (returned false) *)
let unit_test (condition : bool) (msg : string) : unit =
  if condition then
    Printf.printf "%s passed\n" msg
  else
    Printf.printf "%s FAILED\n" msg ;;

(* unit_test_within tolerance value1 value2 msg -- Tests that value1
   is within tolerance of value2. Identifies test using msg. *)
let unit_test_within (tolerance : float)
                     (value1 : float)
                     (value2 : float)
                     (msg : string)
                   : unit =
  unit_test (abs_float (value1 -. value2) < tolerance) msg ;;

let nonincreasing_test () =
  unit_test (nonincreasing [])
            "rev empty";
  unit_test (nonincreasing [7])
            "rev single";
  unit_test (nonincreasing [4;4;4])
            "rev repeat";
  unit_test (not (nonincreasing [2;1;2]))
            "rev inc at start";
  unit_test (nonincreasing [2;2;1])
            "rev dups";
  unit_test (nonincreasing [9;8;7;6;5;5;5;4;4;-2])
            "rev long with neg";
  unit_test (not (nonincreasing [9;8;7;6;7;5;5;5;5;4;3]))
    "rev long inc at mid" ;;

let merge_test () =
  unit_test (merge [] [] = []) "the merging of an empty list to another" ;
  unit_test (merge [] [1 ; 2; 3] = 1 ; 2; 3]) "the merging of one empty list to
another non-empty list";
  unit_test (merge [1 ; 2 ; 3] [] = [1 ;2 ; 3]) "the merging of one non-empty list to
another empty list";
  unit_test (merge [1 ; 3 ; 5] [2 ; 4 ; 6] = [1 ; 2 ; 3 ; 4 ; 5 ; 6])
    "the merging of two lists, same size" ;
unit_test (merge [1 ; 800 ; 3] [4 ; 5] = [1 ; 3 ; 4 ; 5 ; 800])
  "the merging of two lists with an abnormally large value" ;
unit_test (merge [1 ; 5] [3 ; 4 ; 2] = [1 ; 2 ; 3 ; 4 ; 5])
  "the merging of two lists, different sizes" ;

let unzip_test () =
  unit_test (unzip [] = ([], [])) "the unzipping of empty lists";
  unit_test (unzip [(true,false)] = ([true], [false]))
    "the unzipping of a singular pair";
  unit_test (unzip [(true , true); (false , true)] = ([true ; false], [true ; true]))
    "the unzipping of two pairs";
  unit_test (unzip [(true , false) ; (true , false) ; (true , true)] =
             ([true ; true ; true], [false ; false ; true]))
    "the unzipping of three pairs";

  let to_run_length_test () =
    unit_test (to_run_length [] = []) "to_run_length on an empty list"
    unit_test (to_run_length ['a' ; 'a'] = [(2 , 'a')])
    "to_un_length on an list with a single repeated element"
    unit_test (to_run_length ['a' ; 'a' ; 'a' ; 'b' ; 'b' ; 'c' ; 'c' ; 'c' ; 'c']
               = [(3 , 'a') ; (2, 'b') ; (4, 'c')]) "to_run_length on a longer list"
    unit_test (to_run_length ['a' ; 'b' ; 'a'] = [(1 , 'a') ; (1 , 'b') ; (1 , 'a')])
    "to_run_length with a repeated but nonconsecutive value"
    unit_test (to_run_length ['a' ; 'b' ; 'c' ; 'd' ; 'e' ; 'f'] =
               [(1 , 'a') ; (1 , 'b') ; (1 , 'c') ; (1 , 'd') ; (1 , 'e') ; (1 , 'f')])
    "to_run_length on a list with no elements that repeat";

  let from_run_length_test () =
  unit_test (from_run_length [] = []) "from_run_lenght on an empty list";
  unit_test (from_run_length [(3 ,  'a')] = ['a' ; 'a' ; 'a'])
    "from_run_lenght on a list with only one repeated";
  unit_test (from_run_length [(1 , 'a'); (1 , 'b'); (1,  'a')] = ['a' ; 'b' ; 'a'])
    "from_run_lenght with a repeated but nonconsecutive value";
  unit_test (from_run_length [(2 , 'a') ; (3 , 'b') ; (1 , 'a') ; (2 , 'c')] =
             ['a' ; 'a' ; 'b' ; 'b' ; 'b' ; 'a' ; 'c' ; 'c'])
      "from_run_length from a longer list" ;;
  unit_test (from_run_length [(1 , 'a'); (1 , 'b'); (1 , 'c'); (1 , 'd'); (1 , 'e')
                                (1 , 'f')] = ['a' ; 'b' ; 'c' ; 'd' ; 'e' ; 'f'])
  "from_run_lenght list with no repeated elements";

let extract_entry_test () =
  unit_test (extract_entry (cDEFECT, cDEFECT) [] = (404, 404)
    "extract_entry_test when matrix empty" ;;
  unit_test (extract_entry (cCOOPERATE, cCOOPERATE) test_payoff_matrix = (3,  3))
    "extract_entry_test when both parties cooperate";
  unit_test (extract_entry (cCOOPERATE, cDEFECT) test_payoff_matrix = (-2, 5))
      "extract_entry_test when personally cooperate and other defects";
  unit_test (extract_entry (cDEFECT, cCOOPERATE) test_payoff_matrix = (5, -2))
    "extract_entry_test when personally defect and other cooperates";
  unit_test (extract_entry (cDEFECT, cDEFECT) test_payoff_matrix = (0, 0))
    "extract when both parties defect";

let count_defections_test () =
  unit_test (count_defections [] = (0, 0))
  "count_defections_test when history is empty";
  unit_test (count_defections [(cCOOPERATE,  cCOOPERATE)] = (0, 0))
  "count_defections_test with single play history and cooperation from both";
  unit_test (count_defections [(cCOOPERATE,  cDEFECT)] = (0, 1))
  "count_defections_test with single play history and personal cooperation
    and other defection";
  unit_test (count_defections [(cDEFECT,  cDEFECT); (cDEFECT, cCOOPERATE)] = (2, 1))
  "count_defections_test with two plays history";
  unit_test (count_defections [(cCOOPERATE,  cDEFECT); (cCOOPERATE, cDEFECT)]= (0, 2))
  "count_defections_test with multiple plays history and when only one player defects";
  unit_test (count_defections [(cDEFECT,  cDEFECT); (cDEFECT; cCOOPERATE);
        (cDEFECT, cDEFECT); (cDEFECT, cDEFECT); (cCOOPERATE, cDEFECT)] = (4, 4))
    "count_defections_test with longer plays history"

let count_cooperations_test () =
  unit_test (count_cooperations [] = (0, 0))
    "count_cooperations_test when history is empty";
  unit_test (count_cooperations [(cDEFECT, cDEFECT)] = (0, 0))
    "count_cooperations_test with single play history and defection from both";
  unit_test (count_cooperations [(cDEFECT, cCOOPERATE)] = (0, 1))
  "count_cooperations_test with single play history and personal defection
    and other cooperation";
  unit_test (count_cooperations [(cDEFECT, cDEFECT); (cDEFECT, cCOOPERATE)] = (0, 1))
  "count_cooperations_test with two plays history";
  unit_test (count_cooperations [(cCOOPERATE, cDEFECT); (cCOOPERATE, cDEFECT)] = (2, 0))
  "count_cooperations_test with multiple plays history and when only one player cooperates";
  unit_test (count_cooperations [(cDEFECT, cCOOPERATE); (cDEFECT, cCOOPERATE);
                                 (cCOOPERATE, cDEFECT); (cCOOPERATE, cDEFECT);
                                 (cCOOPERATE, cCOOPERATE)] = (3, 3))
  "count_cooperations_test with longer plays history" ;;

let balanced_test () =
  unit_test (balanced [] = cCOOPERATE)
    "balanced_test with first move cooperation";
  unit_test (balanced [(cCOOPERATE, cDEFECT)] = cDEFECT)
    "balanced_test with a defection after cooperating";
  unit_test (balanced [(cDEFECT, cDEFECT); (cCOOPERATE, cDEFECT)] = cCOOPERATE)
    "balanced_test with cooperation after defection";
  unit_test (balanced [(cDEFECT, cDEFECT); (cCOOPERATE, cDEFECT)] = cCOOPERATE)
    "balanced_test when opposite of previous move performed in a history with two plays";
  unit_test (balanced [(cDEFECT, cDEFECT); (cCOOPERATE, cCOOPERATE);
                       (cCOOPERATE, cDEFECT); (cCOOPERATE, cCOOPERATE)] = cCOOPERATE)
    "balanced_test when opposite of previous move performed in long history";;

let egalitarian_test () =
  unit_test (egalitarian [] = cCOOPERATE)
    "egalitarian_test with cooperation on the first move";
  unit_test (egalitarian [(cDEFECT; cDEFECT)] = cCOOPERATE)
    "egalitarian_test with cooperation when defects_count
    is the same for both participants";
  unit_test (egalitarian [(cCOOPERATE, cCOOPERATE); (cCOOPERATE, cCOOPERATE)] = cCOOPERATE)
    "egalitarian_test with cooperation when opponent has no defections";
  unit_test (egalitarian [(cCOOPERATE, cDEFECT); (cCOOPERATE, cCOOPERATE] = cDEFECT)
    "egalitarian_test with defection once opponent first defects";
  unit_test (egalitarian [(cCOOPERATE, cDEFECT); (cDEFECT, cCOOPERATE); (cCOOPERATE, cDEFECT)] = cDEFECT)
    "egalitarian_test with defection when opponent has more defections" ;;

let tit_for_tat_test () =
  unit_test (tit_for_tat [(cCOOPERATE, cDEFECT)] = cDEFECT)
    "tit_for_tat_test with defection when cooperation is opponent's latest move";
  unit_test (tit_for_tat [] = cCOOPERATE)
    "tit_for_tat_test with first move cooperation";
  unit_test (tit_for_tat [(cCOOPERATE, cCOOPERATE)] = cCOOPERATE)
    "tit_for_tat_test with cooperation when cooperation is opponent's latest move";
  unit_test (tit_for_tat [(cCOOPERATE, cCOOPERATE); (cDEFECT, cCOOPERATE);
                          (cCOOPERATE, cDEFECT); (cDEFECT, cCOOPERATE);
                          (cCOOPERATE, cDEFECT)] = cCOOPERATE)
    "tit_for_tat_test where it performs opponent’s previous action, long play history" ;;
  unit_test (tit_for_tat [(cCOOPERATE, cDEFECT); (cCOOPERATE, cCOOPERATE)] = cDEFECT)
    "tit_for_tat_test where opponent’s previous action is performed, two play history";

let my_strategy_test () =
  unit_test (my_strategy [] = cCOOPERATE)
    "my_strategy_test with cooperation on the first move";
  unit_test (my_strategy [(cCOOPERATE, cDEFECT); (cCOOPERATE, cCOOPERATE)] = cCOOPERATE)
    "my_strategy_test with cooperation when opponent has equal coop. and defect counts";
  unit_test (my_strategy [(cCOOPERATE, cCOOPERATE)] = cCOOPERATE)
    "my_strategy_test with cooperationds when first move of opponent is cooperation";
  unit_test (my_strategy [(cCOOPERATE, cDEFECT); (cCOOPERATE, cDEFECT); (cCOOPERATE, cCOOPERATE)] = cDEFECT)
    "my_strategy_test with defection when opponent’s defect count is greater than cooperation count" ;
  unit_test (my_strategy [(cCOOPERATE, cDEFECT)] = cDEFECT)
    "my_strategy_test with defection when first move of opponent is defection" ;;

let swap_actions_test () =
  unit_test (swap_actions [] = [])
    "swap_actions_test with empty history";
  unit_test (swap_actions [(cCOOPERATE, cCOOPERATE)] = [(cCOOPERATE, cCOOPERATE)])
    "swap_actions_test with single play history, identical actions";
  unit_test (swap_actions [(cDEFECT, cCOOPERATE); (cCOOPERATE, cCOOPERATE)] = [(cCOOPERATE, cDEFECT); (cCOOPERATE, cCOOPERATE)] )
    "swap_actions_test with two play history";
  unit_test (swap_actions [(cCOOPERATE, cDEFECT)] = [(cDEFECT, cCOOPERATE)])
    "swap_actions_test with single play history, not the same";
  unit_test (swap_actions [(cDEFECT, cCOOPERATE); (cCOOPERATE, cCOOPERATE); (cCOOPERATE, cDEFECT); (cDEFECT, cCOOPERATE)] = [(cCOOPERATE, cDEFECT); (cCOOPERATE, cCOOPERATE); (cDEFECT, cCOOPERATE); (cCOOPERATE, cDEFECT)])
    "swap_actions_test with a long play history" ;;

let calculate_payoff_test () =
  unit_test (calculate_payoff_test_payoff_matrix [] = (0, 0))
    "calculate_payoff_test with empty history";
  unit_test (calculate_payoff_test_payoff_matrix [(cCOOPERATE, cDEFECT)] = (-2, 5))
    "calculate_payoff_test with single play history";
  unit_test (calculate_payoff_test_payoff_matrix [(cCOOPERATE, cDEFECT); (cDEFECT, cCOOPERATE)] = (3, 3))
    "calculate_payoff_test with two play history";
  unit_test (calculate_payoff_test_payoff_matrix [(cCOOPERATE, cCOOPERATE)] = (3, 3))
    "calculate_payoff_test with cooperation from both parties";
  unit_test (calculate_payoff_test_payoff_matrix [(cDEFECT, cDEFECT)] = (0, 0))
    "calculate_payoff_test with defection from both parties";
  unit_test (calculate_payoff_test_payoff_matrix [(cCOOPERATE, cCOOPERATE);
                                                  (cDEFECT, cDEFECT, cDEFECT);
                                                  (cCOOPERATE, cDEFECT); (cCOOPERATE, cDEFECT);
                                                  (cCOOPERATE, cDEFECT)] = (-3, 18))
    "calculate_payoff_test, long play history";;


let test_all () =
  nonincreasing_test () ;;
  merge_test ();
  unzip_test ();
  to_run_lenght_test ();
  from_run_lenght_test ();
  extract_entry_test ();
  count_defections_test ();
  count_cooperation_test ();
  balanced_test ();
  egalitarian_test ();
  tip_for_tat_test ();
  my_strategy_test ();
  swap_actions_test ();
  calculate_payoff_test ();

let _ = test_all () ;;
