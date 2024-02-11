(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* a *)
fun all_except_option (s, xs) =
  let fun aux ([], fnd, acc) = (fnd, acc)
        | aux (x::xs', fnd, acc) = if same_string(s, x) 
            then aux(xs', true, acc) 
            else aux(xs', fnd, x::acc)
  in 
    case aux(xs, false, []) of
         (false, _) => NONE
       | (true, res) => SOME res
  end
   
(* b *)
fun get_substitutions1 (sll, s) = 
  case sll of
       [] => []
     | sl::sll' => case all_except_option(s, sl) of 
                        SOME rsl => rsl @ get_substitutions1(sll', s)
                      | NONE => get_substitutions1(sll', s)

(* c *)
fun get_substitutions2 (sll, s) = 
  let fun aux([], res) = res
        | aux (sl::sll', res ) = case all_except_option(s, sl) of 
                                      SOME rsl => aux(sll', rsl @ res)
                                    | NONE => aux(sll', res)
  in 
    aux(sll, [])
  end

(* d *)
fun similar_names (name_ls, {first=x,middle=y,last=z}) = 
  let fun aux ([], res) = {first=x,middle=y,last=z}::res
        | aux (f::fs', res) = aux(fs', {first=f,middle=y,last=z}::res)
  in 
    aux (get_substitutions2 (name_ls, x), [])
  end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(* a *)
fun card_color (Spades, _) = Black
  | card_color (Clubs, _) = Black
  | card_color (Diamonds, _) = Red
  | card_color (Hearts, _) = Red

(* b *)
fun card_value (_, Num i) = i
  | card_value (_, Ace) = 11
  | card_value (_, _) = 10

(* c *)
fun remove_card (cs, c, e) = 
  let fun aux ([], fnd, new_cs) = if fnd then new_cs else raise e
        | aux (x::xs', fnd, new_cs) = if x=c andalso not fnd then
            aux (xs', true, new_cs) else
            aux (xs', fnd, x::new_cs)
  in 
    aux (cs, false, [])
  end

(* d *)
fun all_same_color card_list =
  case card_list of 
       [] => true
     | c::[] => true
     | c1::(c2::rest) => (card_color(c1)=card_color(c2) andalso all_same_color (c2::rest))

(* e *)
fun sum_cards card_list =
  let fun aux ([], acc) = acc
        | aux (c::card_list', acc) = aux (card_list', acc + card_value(c))
  in 
    aux (card_list, 0)
  end

(* f *)
fun score (card_list, goal) = 
  let val sum = sum_cards(card_list)
      val same_color = all_same_color(card_list)
      val preliminary_score = if sum > goal then 
        3 * (sum - goal) else goal - sum
  in 
    if same_color then preliminary_score div 2 else preliminary_score 
  end

(* g *)
fun officiate (card_list, move_list, goal) =
  let fun  aux (_, [], held_cards) = held_cards (* no more move *)
    | aux ([], _, held_cards) = held_cards (* no more cards to be drew *)
    | aux (rest_cards, (Discard card)::rest_moves, held_cards) = (* discard move*) 
    aux (rest_cards, rest_moves, remove_card (held_cards, card, IllegalMove))
    | aux (card::rest_cards, Draw::rest_moves, held_cards) = (* draw a card *)
    let val cur_held_cards = card::held_cards
    in 
      if sum_cards cur_held_cards > goal then cur_held_cards
      else aux (rest_cards, rest_moves, cur_held_cards)
    end
  in 
    score (aux (card_list, move_list, []), goal)
  end

(* solutions for challenge problem 3 *)
(* grader output correct follow errors *)
(* score_challenge: Your function returns an incorrect result when the sum is greater than the goal, and the hand contains cards of both colors (ace = 1). [incorrect answer] *)
(* score_challenge: Your function returns an incorrect result when the sum is greater than the goal, and the hand contains cards of only one color. [incorrect answer] *)
(* score_challenge: Your function returns an incorrect result when the input hand is the empty list. [incorrect answer] *)
(* officiate_challenge: Your function returns an incorrect result when the move list empty. [incorrect answer] *)
(* officiate_challenge: Your function returns an incorrect result when an ace is in the players hand. [incorrect answer] *)
(* careful_player: Your function returns an incorrect result when given a hand of [(Spades,Num 7),(Hearts,King),(Clubs,Ace),(Diamonds,Num 2)] and a goal of 18 [incorrect answer] *)
(* careful_player: Your function returns an incorrect result when given a hand of [(Diamonds,Num 2),(Clubs,Ace)] and a goal of 11 [incorrect answer] *)

(* a *)
fun score_challenge (card_list, goal) = 
  let 
    fun score_aux (card_val, cur_sum) =
      let val sum = card_val + cur_sum
      in 
        if sum > goal then 3 * (sum - goal) else goal - sum 
      end
    fun aux ([], _, score) = score
        | aux ((_, Ace)::card_list, sum, score) = 
        let val score_ace_1 = score_aux (1, sum)
            val score_ace_11 = score_aux (11, sum)
        in if score_ace_1 < score_ace_11 then 
          aux (card_list, 1+sum, score_ace_1) 
           else aux (card_list, 11+sum, score_ace_11)
        end
        | aux (other_card::card_list, sum, score) = 
        let 
          val card_val = card_value(other_card)
        in  
          aux (card_list, card_val+sum, score_aux (card_val, sum))
        end
    val same_color = all_same_color(card_list)
  in 
    if same_color then aux (card_list, 0, 0) div 2 else aux (card_list, 0, 0) 
  end

fun officiate_challenge (card_list, move_list, goal) =
  let fun  aux (_, [], held_cards) = held_cards (* no more move *)
    | aux ([], _, held_cards) = held_cards (* no more cards to be drew *)
    | aux (rest_cards, (Discard card)::rest_moves, held_cards) = (* discard move*) 
    aux (rest_cards, rest_moves, remove_card (held_cards, card, IllegalMove))
    | aux (card::rest_cards, Draw::rest_moves, held_cards) = (* draw a card *)
    let val cur_held_cards = card::held_cards
    in 
      if sum_cards cur_held_cards > goal then cur_held_cards
      else aux (rest_cards, rest_moves, cur_held_cards)
    end
  in 
    score_challenge (aux (card_list, move_list, []), goal)
  end

(* b *)
fun careful_player (card_list, goal) = 
  let 
    fun if_drawn (held_cards) = 
      let val sum = sum_cards held_cards
      in goal - sum >= 10
      end
    fun discard_card ([], _, drawn_card) = NONE
      | discard_card (dc::held_cards, diff, drawn_card) = 
      if card_value(dc) = diff then SOME dc else discard_card (held_cards, diff, drawn_card)
    fun play ([], _, move_list) = move_list
      | play (c::card_list, held_cards, move_list) = 
      if score (held_cards, goal) = 0 then move_list
      else 
        case discard_card (held_cards, goal - sum_cards(c::held_cards), c) of
             NONE => play(card_list, c::held_cards, Draw::move_list)
           | SOME dc => play(card_list, c::remove_card(held_cards, dc,
             IllegalMove), Draw::Discard dc::move_list)
  in 
    play (card_list, [],[])
  end
    
