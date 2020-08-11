module Player

open Hand
open Deck

type Player = {id:int; hand:Hand}

let create id deck =
  Deck.drawMultiple deck 2 |> Option.map(fun(hand, deck)->({id=id; hand=hand},deck))

let createMultiple number deck = 
  let initialState = Some([], deck)
  let rec builder number state =
    if number = 0 then state
    else
      match state with
      | None -> None
      | Some(players, deck) ->
        match create number deck with
        | None -> None
        | Some(player, deck) -> 
          let newState = Some(players@[player], deck)
          builder (number-1) newState
  builder number initialState
  