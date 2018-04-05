open List

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal
(* 
  1. To warm up, notice that the format of grammars is different in this assignment, versus Homework 1. 
  Write a function convert_grammar gram1 that returns a Homework 2-style grammar, which is converted 
  from the Homework 1-style grammar gram1. Test your implementation of convert_grammar on the test grammars 
  given in Homework 1. For example, the top-level definition let awksub_grammar_2 = convert_grammar awksub_grammar 
  should bind awksub_grammar_2 to a Homework 2-style grammar that is equivalent to the Homework 1-style 
  grammar awksub_grammar. 
*)

let rec get_rules_by_key rules key = match rules with
  [] -> []
  | (lhs, rhs)::t -> if key = lhs then rhs :: (get_rules_by_key t key) else (get_rules_by_key t key)

let rec convert_grammar gram1 = match gram1 with
  (start, rules) -> (start, (fun lhs -> (get_rules_by_key rules lhs)))

(* 
  2. Write a function parse_prefix gram that returns a matcher for the grammar gram. When applied to an 
  acceptor accept and a fragment frag, the matcher must return the first acceptable match of a prefix of 
  frag, by trying the grammar rules in order; this is not necessarily the shortest nor the longest acceptable 
  match. A match is considered to be acceptable if accept succeeds when given a derivation and the suffix
  fragment that immediately follows the matching prefix. When this happens, the matcher returns whatever the 
  acceptor returned. If no acceptable match is found, the matcher returns None.
*)

let rec match_frag get_rules start rules acceptor deriv frag = match rules with
  [] -> None
  | h::t -> 
    let rec match_in_rule get_rules rule acceptor deriv frag = match rule with
      [] -> (acceptor deriv frag)
      | _ -> 
        if (frag == []) then None 
        else match rule with
          [] -> None
          | (T x)::rhs -> if x = (hd frag) then (match_in_rule get_rules rhs acceptor deriv (tl frag)) else None
          | (N y)::rhs -> (match_frag get_rules y (get_rules y) (match_in_rule get_rules rhs acceptor) deriv frag)
    in
    let result = (match_in_rule get_rules h acceptor (deriv@[(start, h)]) frag) in
      match result with
        None -> (match_frag get_rules start t acceptor deriv frag)
        | Some res -> result
  
let rec matcher get_rules start acceptor frag = 
    (match_frag get_rules start (get_rules start) acceptor [] frag)

let parse_prefix gram = match gram with
  (start, rules_fn) -> (fun acceptor frag -> (matcher rules_fn start acceptor frag))
