datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* a *)
fun card_color(c : card)=
  case c of
    (Clubs, _) => Black
    | (Diamonds, _) => Red
    | (Hearts, _) => Red
    | (Spades, _) => Black

(* b *)
fun card_value(c : card)=
  case c of
    (_, Jack) => 10
    | (_, Queen) => 10
    | (_, King) => 10
    | (_, Ace) => 11
    | (_, Num n) => n

(* c *)
fun remove_card (cs : card list, c : card, e) =
    case cs of
      [] => raise e
      | hd::tl => if c = hd
                  then tl
                  else hd::remove_card(tl, c, e)

(* e *)
fun sum_cards (cs : card list) =
  let
    fun aux_sum (current_sum, tl_cs : card list) =
      case tl_cs of
        [] => current_sum
        | hd::tl => aux_sum(current_sum + card_value(hd), tl)
  in
    aux_sum(0, cs)
  end

val test1 = card_color (Clubs, Num 2) (* = Black *)
val test2 = card_value (Clubs, Num 2) (* = 2 *)
val test3 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) (* = [] *)

val test5 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] (* = 4 *)

(*
val test4 = all_same_color [(Hearts, Ace), (Hearts, Ace)] (* = true *)

val test6 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) (* = 4 *)
val test7 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) (* = 6 *)
val test8 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw], 42) (* = 3 *)
*)
