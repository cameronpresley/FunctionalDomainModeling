module Turn
open Game
open Points
open Status
open Dealer
open Player
open Action
open Hand

let rec dealerTakesTurn game =
  let dealer = game.dealer
  let deck = game.deck
  match Points.calculateHand dealer.hand with
  | Hard x when x >= 17 -> Some {game with dealer=dealer; deck=deck}
  | Soft (_,x) when x >= 17 -> Some {game with dealer=dealer; deck=deck}
  | _ ->
    match Deck.draw deck with
    | None -> None
    | Some(card, deck) ->
      let dealer = {dealer with hand=dealer.hand@[card]}
      dealerTakesTurn {game with dealer=dealer; deck=deck}

let rec playerTakesTurn player deck getOption =
  match Points.calculateHand player.hand with
  | Soft(_,x) when x = 21 && player.hand.Length=2 -> Some(player,deck)
  | Hard x when x > 21 -> Some (player,deck)
  | Soft(x,_) when x > 21 -> Some (player,deck)
  | _ ->
    match getOption player with
    | Action.Stay -> Some (player,deck)
    | Action.Hit ->
      match Deck.draw deck with
      | None -> None
      | Some(card, deck) ->
        let player = {player with hand = player.hand@[card]}
        playerTakesTurn player deck getOption

let playersTakeTurn getAction game =
  let initialState = Some ([], game.deck)
  let folder state player =
    match state with
    | None -> None
    | Some(players,deck) ->
      match playerTakesTurn player deck getAction with
      | None -> None
      | Some(player,deck) -> Some (players@[player], deck)

  game.players 
    |> List.fold folder initialState
    |> Option.map (fun (players,deck) -> {game with players=players; deck=deck})

let rec promptPlayer player = 
  printfn "Player %i has %s" player.id (player.hand |> List.map Card.format |> List.reduce (+))
  printfn "Choose H to hit"
  printfn "Choose S to stay"
  let input = System.Console.ReadLine()
  match input with
  | "s" | "S" -> Action.Stay
  | "h" | "H" -> Action.Hit
  | _ -> 
        printfn "That's not a valid choice"
        promptPlayer player