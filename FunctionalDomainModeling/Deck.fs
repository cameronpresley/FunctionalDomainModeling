module Deck
open Card
open MaybeBuilder

type Deck = Deck of Card list

let create () = 
 let ranks = [Ace; Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King]
 let suits = [Hearts; Clubs; Spades; Diamonds]
 let tupleToCard (r:Rank, s:Suit) = {Rank=r; Suit=s}

 List.allPairs ranks suits |> List.map tupleToCard

let shuffle (sorter:(Card -> int)) = 
  () |> create |> List.sortBy sorter |> Deck

let draw (Deck d) = 
  match d with
  | [] -> None
  | card::restOfDeck -> Some(card, (Deck)restOfDeck)

let drawMultiple' deck number = 
  let rec buildHand (num:int) (state:(Card list * Deck) option) =
    match num with
    | 0 -> state
    | _ ->
      match state with
      | None -> None
      | Some (cards, deck) ->
        match draw deck with
        | None -> None
        | Some (card, deck) -> 
          buildHand (num-1) (Some(cards@[card], deck))

  buildHand number (Some([], deck))

let drawMultiple deck number =
  let rec buildHand (num:int) (state:(Card list * Deck) option) =
    match num with
    | 0 -> state
    | _ -> 
      let newState = maybe{
        let! cards,deck = state
        let! card,deck = draw deck
        return (cards@[card], deck)
      }
      buildHand (num-1) newState
  
  buildHand number (Some([], deck))