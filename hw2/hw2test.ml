let accept_all derivation string = Some (derivation, string)

type awksub_nonterminals =
  | Expr | Term | Lvalue | Incrop | Binop | Num

let awkish_grammar =
  (Expr,
   function
     | Expr ->
         [[N Term; N Binop; N Expr];
          [N Term]]
     | Term ->
	 [[N Num];
	  [N Lvalue];
	  [N Incrop; N Lvalue];
	  [N Lvalue; N Incrop];
	  [T"("; N Expr; T")"]]
     | Lvalue ->
	 [[T"$"; N Expr]]
     | Incrop ->
	 [[T"++"];
	  [T"--"]]
     | Binop ->
	 [[T"+"];
	  [T"-"]]
     | Num ->
	 [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
	  [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]])

let test_1 =
  (parse_prefix awkish_grammar accept_all ["++"; "$"; "8"])
    = Some ([(Expr, [N Term]); (Term, [N Incrop; N Lvalue]); (Incrop, [T "++"]);
            (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Num]);      
            (Num, [T "8"])], [])

type math_nonterminals =
  | S | A | M | Num

let my_gram =
  (S,
   function
     | S ->
         [[N A]]
     | A ->
         [
           [N M; T "+"; N A];
           [N M]
         ]
     | M ->
         [
           [N Num; T "*"; N M];
           [N Num]
         ]
     | Num ->
          [
            [T "1"];
            [T "2"];
            [T "3"];
            [T "4"]
          ]
  )

let test_2 =
  (parse_prefix my_gram accept_all ["4"; "+"; "2"; "*"; "3"])
  = Some ([(S, [N A]); (A, [N M; T "+"; N A]); (M, [N Num]); (Num, [T "4"]);  
          (A, [N M]); (M, [N Num; T "*"; N M]); (Num, [T "2"]); (M, [N Num]);
          (Num, [T "3"])], [])                                                                 
  