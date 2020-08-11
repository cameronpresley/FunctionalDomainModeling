module Dealer

open Hand
open Deck

type Dealer = { Hand: Hand }

let create d =
    match draw d with
    | None -> None
    | Some (card, d) ->
        match draw d with
        | None -> None
        | Some (card', d) -> Some({ Hand = [ card; card' ] }, d)
