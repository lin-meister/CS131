open List

(* 
  1. Write a function subset a b that returns true iff a⊆b,
  i.e., if the set represented by the list a is a subset 
  of the set represented by the list b. Every set is a 
  subset of itself. This function should be generic to 
  lists of any type: that is, the type of subset should 
  be a generalization of 'a list -> 'a list -> bool. 
*)
let rec subset a b = match a with
  [] -> true
  | h::t -> if b = [] then false else (mem h b) && (subset t b)

(*
  2. Write a function equal_sets a b that returns true iff 
  the represented sets are equal.
*)
let rec equal_sets a b =
  subset a b && subset b a

(*
  3. Write a function set_union a b that returns a list representing a∪b.
*)
(* Extract set from list *)
let rec make_set lst s = match lst with
  [] -> s
  | h::t -> if (not (mem h s)) then (make_set t (h::s)) else (make_set t s)

let rec set_union a b =
  make_set b (make_set a [])
  
(*
  4. Write a function set_intersection a b that returns a list representing a∩b.
*)
let rec set_intersection a b = match a with
  [] -> []
  | h::t -> if (mem h b) then h::(set_intersection t b) else (set_intersection t b)

(* 
  5. Write a function set_diff a b that returns a list representing a−b, that is, 
  the set of all members of a that are not also members of b. 
*)
let rec set_diff a b = match a with
  [] -> []
  | h::t -> if (not (mem h b)) then h::(set_diff t b) else (set_diff t b)
  
(* 
  6. Write a function computed_fixed_point eq f x that returns the computed fixed point 
  for f with respect to x, assuming that eq is the equality predicate for f's domain. 
  A common case is that eq will be (=), that is, the builtin equality predicate of OCaml; 
  but any predicate can be used. If there is no computed fixed point, your implementation 
  can do whatever it wants: for example, it can print a diagnostic, or go into a loop, or 
  send nasty email messages to the user's relatives. 
*)
(* c is a fixed point of the function f(x) if f(c) eq c. *)
let rec computed_fixed_point eq f x = 
  if eq (f x) x then x else (computed_fixed_point eq f (f x))

(*
  7. Write a function computed_periodic_point eq f p x that returns the computed periodic point 
  for f with period p and with respect to x, assuming that eq is the equality predicate for 
  f's domain.
*)
(* Helper function for computing periodic point *)
let rec computed_periodic_point_helper eq f p x f_x = match p with
  0 -> eq x f_x
  | _ -> (computed_periodic_point_helper eq f (p - 1) x (f f_x))

let rec computed_periodic_point eq f p x = 
  if (computed_periodic_point_helper eq f p x x) then x else (computed_periodic_point eq f p (f x))

(* 
  8. Write a function while_away s p x that returns the longest list [x; s x; s (s x); ...] 
  such that p e is true for every element e in the list. That is, if p x is false, return []; 
  otherwise if p (s x) is false, return [x]; otherwise if p (s (s x)) is false, return [x; s x]; 
  and so forth. For example, while_away ((+) 3) ((>) 10) 0 returns [0; 3; 6; 9]. Your implementation 
  can assume that p eventually returns false. 
*)

let rec while_away s p x = match (p x) with
  false -> []
  | true -> x :: (while_away s p (s x))

(*
  9. Write a function rle_decode lp that decodes a list of pairs lp in run-length encoding form. 
  The first element of each pair is a nonnegative integer specifying the repetition length; the second 
  element is the value to repeat. For example, rle_decode [2,0; 1,6] should return [0; 0; 6] and 
  rle_decode [3,"w"; 1,"x"; 0,"y"; 2,"z"] should return ["w"; "w"; "w"; "x"; "z"; "z"].
*)
(* Repeat a value v by len times *)
let rec repeat len v = match len with
  0 -> []
  | _ -> v :: (repeat (len - 1) v)

let rec rle_decode lp = match lp with
  [] -> []
  | h::t -> let (a, b) = h in (repeat a b) @ (rle_decode t)

(*
  10. OK, now for the real work. Write a function filter_blind_alleys g that returns a copy of the 
  grammar g with all blind-alley rules removed. This function should preserve the order of rules: 
  that is, all rules that are returned should be in the same order as the rules in g. 
*)

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal
  
(* Check if a symbol is terminal *)
let rec is_terminal symb = match symb with 
  T _ -> true
  | _ -> false
  
(* Check if a symbol is non_terminal but eventually generates terminal  *)
let rec is_good_non_terminal symb non_blind_rules = match non_blind_rules with
  [] -> false
  | h::t -> if symb = (N (fst h)) then true else (is_good_non_terminal symb t)

(* Check if the symbols on rhs indicate a non blind alley rule *)
let rec is_non_blind rhs non_blind_rules = match rhs with
  [] -> true
  | h::t -> if (is_terminal h) || (is_good_non_terminal h non_blind_rules) then (is_non_blind t non_blind_rules) else false

(* Extract the non blind alley rules *)
let rec get_non_blind_rules rules non_blind_rules = match rules with
  [] -> non_blind_rules
  | h::t -> if (is_non_blind (snd h) non_blind_rules) then (get_non_blind_rules t (h::[])@non_blind_rules) else (get_non_blind_rules t non_blind_rules)

(* Repeat process every time while building up list of non blind alley rules/terminal symbols *)
let rec filter_rules rules non_blind_rules =
  let new_non_blind_rules = (get_non_blind_rules rules non_blind_rules) in
    if new_non_blind_rules = non_blind_rules then non_blind_rules 
    else (filter_rules (set_diff rules new_non_blind_rules) new_non_blind_rules)

let rec filter_blind_alleys g = match g with
  (start, rules) -> (start, (set_intersection rules (filter_rules rules []))) (* set_intersection for maintaining order *)
