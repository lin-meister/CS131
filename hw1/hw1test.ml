let my_subset_test0 = subset [] []
let my_subset_test1 = subset [1;2] [1;2;3]
let my_subset_test2 = not (subset [1;2] [1])

let my_equal_sets_test0 = equal_sets [] []
let my_equal_sets_test1 = not (equal_sets [] [1;2;3])
let my_equal_sets_test2 = not (equal_sets [1;2;3] [])
let my_equal_sets_test3 = equal_sets [1] [1]
let my_equal_sets_test4 = not (equal_sets [1] [1;2])
let my_equal_sets_test5 = not (equal_sets [1;2] [1])

let my_set_union_test0 = equal_sets (set_union [] [1;2]) [1;2]
let my_set_union_test1 = equal_sets (set_union [2;3] [1;2]) [1;2;3]

let my_set_intersection_test0 = equal_sets (set_intersection [] []) []
let my_set_intersection_test1 = equal_sets (set_intersection [1] []) []
let my_set_intersection_test2 = equal_sets (set_intersection [1;4] [2;3;4]) [4]

let my_set_diff_test0 = equal_sets (set_diff [1;3] [1;2;3;4;5]) []
let my_set_diff_test1 = equal_sets (set_diff [1;3;6] [1;2;3;4;5]) [6]
let my_set_diff_test2 = equal_sets (set_diff [] [1;2;3;4;5]) []
let my_set_diff_test3 = equal_sets (set_diff [1;2;3] []) [1;2;3]

let my_computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x / 3) 1000000000 = 0
let my_computed_fixed_point_test1 =
  computed_fixed_point (=) (fun x -> x * x) 10 = 0

let my_computed_periodic_point_test0 =
  computed_periodic_point (=) (fun x -> x) 0 (1) = 1

let my_while_away_test0 = 
  equal_sets (while_away ((+) 5) ((>) 20) 0) [0; 5; 10; 15]

let my_rle_decode_test0 = equal_sets (rle_decode [2,1; 2,2]) [1; 1; 2; 2]
let my_rle_decode_test1 =
    equal_sets (rle_decode [3,"x"; 1,"y"; 2,"z"]) ["x"; "x"; "x"; "y"; "z"; "z"]
    
type nonterminals =
  | S | A | B | C

let my_grammar = 
  S,
  [S, [N A];
    S, [N B];
    A, [T "a"];
    A, [N A];
    B, [N B; N C]]

let my_filter_blind_alleys_test0 = 
  filter_blind_alleys my_grammar = 
    (S,
    [S, [N A];
      A, [T "a"];
      A, [N A]])