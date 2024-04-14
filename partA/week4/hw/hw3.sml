(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* 1 *)
fun only_capitals str_ls =
  List.filter (fn s => Char.isUpper(String.sub(s, 0))) str_ls;

(* 2 *)
fun longest_string1 str_ls = 
  List.foldl (fn (curr_str, longest_str) => if String.size(curr_str) >
  String.size(longest_str) then curr_str else longest_str) "" str_ls

(* 3 *)
fun longest_string2 str_ls = 
  List.foldl (fn (curr_str, longest_str) => if String.size(curr_str) >=
  String.size(longest_str) then curr_str else longest_str) "" str_ls

(* 4 *)
fun longest_string_helper f_cmp str_ls =
  List.foldl
  (
    fn (curr_str, longest_str) =>
      if f_cmp(String.size(curr_str), String.size(longest_str)) 
      then curr_str else longest_str
  ) "" str_ls
  
val longest_string3 = longest_string_helper (fn (a,b) => a > b)

val longest_string4 = longest_string_helper (fn (a,b) => a >= b)

(* 5 *)
val longest_capitalized = longest_string1 o only_capitals

(* 6 *)
val rev_string = implode o rev o explode

(* 7 *)
fun first_answer f xs = 
  case xs of
    [] => raise NoAnswer
    | x::xs' => case f(x) of
                NONE=> first_answer f xs'
               |SOME ans => ans

(* 8 *)
fun all_answers f xs = 
  let
    fun aux_fold([], []) = NONE
      | aux_fold(acc, []) = SOME acc
      | aux_fold(acc, x::xs') = case f x of
                                     SOME v => aux_fold(v@acc, xs')
                                   | _ => aux_fold(acc, xs')
  in 
    case xs of
         [] => SOME []
       | _ => aux_fold([], xs)
  end

(* g = fn : (unit -> unit) -> (string -> int) -> pattern -> int*)
(* 9(a) *)
val count_wildcards = g (fn _ => 1) (fn _ => 0) 
(* count_wildcards = fn : pattern -> int *)

(* 9(b) *)
val count_wild_and_variable_lengths = g (fn _ => 1) String.size

(* 9(c) *)
fun count_some_var (s, p) = g (fn _ => 0) (fn ps => if ps = s then 1 else 0) p
(* count_some_var = fn : string * patern -> int *)

(* 10 *)
fun check_pat p =
  let 
    fun list_all_strings p =
      case p of
           Variable s => [s]
         | TupleP ps => List.foldl (fn (ps', curr_ls) =>
             list_all_strings(ps')@curr_ls) [] ps
         | _ => []
    fun check_distinct xs = 
      case xs of
           [] => true
         | x::xs' => if List.exists (fn s => s = x) xs' then false else check_distinct xs'
  in
    check_distinct(list_all_strings p)
  end

(* 11 *)
fun match (v, p) =
  case (v, p) of
       (_, Wildcard) => SOME []
     | (_, Variable s) => SOME [(s, v)]
     | (Unit, UnitP) => SOME []
     | (Const c, ConstP cp) => if c = cp then SOME [] else NONE
     | (Constructor(sv, cv), ConstructorP(sp, cp)) => if sv = sp then match (cv, cp) else NONE
     | (Tuple vs, TupleP ps) => if List.length vs = List.length ps
                                then 
                                  case all_answers match (ListPair.zip(vs,ps)) of
                                       SOME res => SOME res
                                     | _ => NONE
                                else NONE
     | (_, _) => NONE

(* 12 *)
fun first_match v ps =
  SOME (first_answer (fn p => match (v, p)) ps)
  handle NoAnswer => NONE

(* challenge *)
(* fun typecheck_patterns (datatypes, ps) =  *)

