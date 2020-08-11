module Turn
open Game
open Points
open Dealer
open Player
open Action

let rec dealerTakesTurn game =
  let dealer = game.Dealer
  let deck = game.Deck
  match Points.calculateHand dealer.Hand with
  | Hard x when x >= 17 -> Some {game with Dealer=dealer; Deck=deck}
  | Soft (_,x) when x >= 17 -> Some {game with Dealer=dealer; Deck=deck}
  | _ ->
    match Deck.draw deck with
    | None -> None
    | Some(card, deck) ->
      let dealer = {dealer with Hand=dealer.Hand@[card]}
      dealerTakesTurn {game with Dealer=dealer; Deck=deck}

let rec playerTakesTurn player deck getOption =
  match calculateHand player.Hand with
  | Soft(_,x) when x = 21 && player.Hand.Length=2 -> Some(player,deck)
  | Hard x when x > 21 -> Some (player,deck)
  | Soft(x,_) when x > 21 -> Some (player,deck)
  | _ ->
    match getOption player with
    | Stay -> Some (player,deck)
    | Hit ->
      match Deck.draw deck with
      | None -> None
      | Some(card, deck) ->
        let player = {player with Hand = player.Hand@[card]}
        playerTakesTurn player deck getOption

let playersTakeTurn getAction game =
  let initialState = Some ([], game.Deck)
  let folder state player =
    MaybeBuilder.maybe {
      let! players,deck = state
      let! player,deck = playerTakesTurn player deck getAction
      return (players@[player], deck)
    }
  game.Players 
    |> List.fold folder initialState
    |> Option.map (fun (players,deck) -> {game with Players=players; Deck=deck})

let rec promptPlayer player = 
  printfn "Player %i has:" player.Id 
  printfn "%s" (player.Hand |> List.map Card.format |> List.reduce (+))
  printfn "Choose H to hit"
  printfn "Choose S to stay"
  let input = System.Console.ReadLine()
  match input with
  | "s" | "S" -> Action.Stay
  | "h" | "H" -> Action.Hit
  | _ -> 
        printfn "That's not a valid choice"
        promptPlayer player