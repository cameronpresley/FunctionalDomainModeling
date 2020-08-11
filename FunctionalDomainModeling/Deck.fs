module Deck
open Card
open Rank
open Suit
open MaybeBuilder

type Deck = private Deck of Card list

let create () = 
 let ranks = [Ace; Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King]
 let suits = [Hearts; Clubs; Spades; Diamonds]
 let tupleToCard (r:Rank, s:Suit) = {rank=r; suit=s}

 List.allPairs ranks suits |> List.map tupleToCard

let shuffle (sorter:(Card -> int)) = 
  () |> create |> List.sortBy sorter |> Deck

let draw (Deck d) = 
  match d with
  | [] -> None
  | h::t -> Some(h, (Deck)t)

// let drawMultiple d number = 
//   let rec buildHand (number:int) (state:(Card list * Deck) option) =
//     if number = 0 then state
//     else
//       match state with
//       | None -> None
//       | Some (cards, restOfDeck) ->
//         match draw restOfDeck with
//         | None -> None
//         | Some (card, restOfDeck) ->
//           buildHand (number-1) (Some(cards@[card], restOfDeck))

//   buildHand number (Some([], d))

let drawMultiple d number =
  let rec buildHand (number:int) (state:(Card list * Deck)option) =
    if number = 0 then state
    else
      state
      |> Option.bind(fun((cards, deck)) -> 
                        draw deck |> Option.map(fun(card, deck) -> (cards@[card], deck)))
      |> buildHand (number-1)
  buildHand number (Some([], d))
