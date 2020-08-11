module Player

open Hand
open MaybeBuilder

type Player = { Id: int; Hand: Hand }

let create id deck =
    Deck.drawMultiple deck 2
    |> Option.map (fun (hand, deck) -> ({ Id = id; Hand = hand }, deck))

let createMultiple number deck =
    let initialState = Some([], deck)

    let rec builder number state =
        if number = 0 then
            state
        else
          let newState = maybe {
            let! players,deck = state
            let! player,deck = create number deck
            return (players@[player], deck) 
          }
          builder (number-1) newState

    builder number initialState
