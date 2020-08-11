module Dealer
open Hand
open Deck

type Dealer = {hand:Hand}

let create d = 
  match draw d with
  | None -> None
  | Some (card, d) ->
    match draw d with
    | None -> None
    | Some (card', d) -> Some ({hand=[card;card']}, d)