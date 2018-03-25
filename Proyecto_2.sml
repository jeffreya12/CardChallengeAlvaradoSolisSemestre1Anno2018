datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* a *)
fun card_color(c : card)=
  (* Clubs y Spades son negras, Diamonds y Hearts son rojas *)
  case c of
    (Clubs, _) => Black
    | (Diamonds, _) => Red
    | (Hearts, _) => Red
    | (Spades, _) => Black

(* b *)
fun card_value(c : card)=
  (* Jack, Queen y King valen 10, Ace 11 y las demas tienen su propio valor *)
  case c of
    (_, Jack) => 10
    | (_, Queen) => 10
    | (_, King) => 10
    | (_, Ace) => 11
    | (_, Num n) => n

(* c *)
fun remove_card (cs : card list, c : card, e) =
  case cs of
    (* Si la lista esta vacia es porque no encontro la carta *)
    [] => raise e (* Se lanza la excepcion *)
    (* Si tiene al menos un elemento *)
    | hd::tl => if c = hd (* Si el primer elemento es el que se va a quitar *)
                then tl (* Devuelve la cola *)
                else hd::remove_card(tl, c, e) (* Si no, mantiene el elemento
                                                  y llama recursivamente con la
                                                  cola de la lista *)

(* d *)
fun all_same_color (cs : card list) =
  case cs of
    [] => false (* Si la lista esta vacia, retorna false *)
    | hd::[] => true (* Si la lista tiene solo un elemento, retorna true *)
    (* Si la lista tiene tiene dos elementos, los compara y retorna el
       resutado de la comparacion *)
    | hd::mid::[] => card_color (hd) = card_color (mid)
    (* Si la lista tiene mas de dos elementos, compara los primeros dos y aplica
       un AND con la llamada recursiva a la funcion con el resto de la lista *)
    | hd::mid::tl => card_color (hd) = card_color (mid) andalso all_same_color(tl)

(* e *)
fun sum_cards (cs : card list) =
  let
    (* Funcion auxiliar que realiza la suma de la lista *)
    fun aux_sum (current_sum, tl_cs : card list) =
      case tl_cs of
        [] => current_sum (* Si la lista es vacia, retorna la ultima suma *)
        (* Si la lista tiene elementos, hace una llamada a la funcion con la
           ultima suma calculada sumada a la cabeza de la lista y el resto
           de la lista *)
        | hd::tl => aux_sum(current_sum + card_value(hd), tl)
  in
    (* La primera suma siempre es 0 *)
    aux_sum(0, cs)
  end

(* f *)
fun score (held_cards : card list, goal) =
  let
    (* Suma de todos los valores de las held_cards *)
    val sum = sum_cards (held_cards)
    (* Resultado preliminar *)
    val preliminary_score = if sum > goal
                            (* Si la suma es mayor al objetivo, entonces el
                              resultado preliminar es 3 veces la suma menos el
                              objetivo *)
                            then 3 * (sum - goal)
                            (* En caso contrario, el resultado preliminar es
                              el objetivo menos la suma *)
                            else (goal - sum)
  in
    (* Si todos son del mismo color *)
    if all_same_color (held_cards)
    then preliminary_score div 2 (* El resultado final seria el preliminar
                                    entre 2*)
    else preliminary_score (* Si no todas son del mismo color, el resultado
                              preliminar es el final *)
  end

(* g *)
fun officiate(cards : card list, moves : move list, goal : int) =
  let
    (* Funcion auxiliar que lleva las held_cards *)
    fun play(remaining_cards : card list, held_cards : card list, remaining_moves : move list) =
      case (remaining_cards, held_cards, remaining_moves) of
        (* Si no quedan movimientos, termina el juego *)
        ( _, _, []) => score(held_cards, goal)
        (* Si no quedan cartas por tomar, termina el juego *)
        | ([], _, Draw::_) => score(held_cards, goal)
        (* Se descarta una carta y no quedan jugadas, termina el juego *)
        | (remaining_cards, held_cards, Discard c::[]) => score (remove_card(held_cards, c, IllegalMove), goal)
        (* Se toma una carta y no quedan jugadas, termina el juego *)
        | (top::_, held_cards, Draw::[]) => score(top::held_cards, goal)
        (* Se descarta una carta y quedan jugadas, juego sigue.
          remaining_cards -> se mantiene
          held_cards -> pierde una carta
          remaining_moves -> se juega el resto de la lista *)
        | (remaining_cards, held_cards, Discard c::tl_remaining_moves) => (play(remaining_cards, remove_card(held_cards, c, IllegalMove), tl_remaining_moves))
        (* Se toma una carta y quedan jugadas, juego sigue a menos que la suma
          de las held_cards sea mayor a goal.
          remaining_cards -> pierde una carta
          held_cards -> suma una carta
          remaining_moves -> se juega el resto de la lista *)
        | (remaining_cards, held_cards, Draw::tl_remaining_moves) => let
                                                                        (* Tope de la pila de cartas *)
                                                                        val top::_ = remaining_cards
                                                                        (* Resultado preliminar antes de verificar la suma
                                                                          de las held_cards*)
                                                                        val preliminary_score = score(top::held_cards, goal)
                                                                      in
                                                                        (* Si la suma de las held_cards es mayor a goal *)
                                                                        if(preliminary_score > goal)
                                                                        then preliminary_score (* entonces retorna el resultado
                                                                                                  preliminar como final *)
                                                                        (* Si no es mayor, el juego sigue con una carta menos
                                                                          en la pila, una carta mas en las held_cards y el
                                                                          siguiente movimiento *)
                                                                        else play(remove_card(remaining_cards, top, IllegalMove), top::held_cards, tl_remaining_moves)
                                                                      end
  in
    (* Se llama a la funcion auxiliar con la pila de cartas, las held_cards
      vacias y la lista de movimientos *)
    play(cards, [], moves)
  end


val test1 = card_color (Clubs, Num 2) (* = Black *)
val test2 = card_value (Clubs, Num 2) (* = 2 *)
val test3 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) (* = [] *)
val test4 = all_same_color [(Hearts, Ace), (Hearts, Ace)] (* = true *)
val test5 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] (* = 4 *)
val test6 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) (* = 4 *)
val test7 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) (* = 6 *)
val test8 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw], 42) (* = 3 *)
