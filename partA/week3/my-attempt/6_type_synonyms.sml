(* use as Enum (enumerating over non-inclusive options) *)
datatype suit = Club | Diamond | Heart | Spade

(* ML allows variants to carry data *)
datatype rank = Jack | Queen | King | Ace | Num of int

val comb = (Club, Queen) (* type checking: suit * rank *)

(* type synonyms *)
type card = suit * rank

val new_card = (Diamond, King)
