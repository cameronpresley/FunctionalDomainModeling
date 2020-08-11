module Outcome

open Points
open Status
open Player
open Dealer
open Game

type Outcome = PlayerBlackjack | DealerBlackjack | PlayerWon | DealerWon | Push

let determineStatus hand = 
  match Points.calculateHand hand with
  | Soft (x, y) when y = 21 && (hand.Length = 2) -> Blackjack
  | Hard x when x > 21 -> Busted x
  | Hard x when x <= 21 -> Stayed x
  | Soft (x, y) when y > 21 && x <= 21 -> Stayed x
  | Soft (x, y) when y <= 21 -> Stayed y
  | Soft (x, y) when y > 21 && x > 21 -> Busted x

let determineOutcome (dealer:Dealer) (player:Player) =
  match determineStatus (dealer.hand), determineStatus(player.hand) with
  | Blackjack, Blackjack -> Push
  | Blackjack, _ -> DealerBlackjack
  | _, Blackjack -> PlayerBlackjack
  | _, Busted _ -> DealerWon
  | Busted _, _ -> PlayerWon
  | Stayed x, Stayed y when x = y -> Push
  | Stayed x, Stayed y when x > y -> DealerWon
  | Stayed x, Stayed y when x < y -> PlayerWon

let determineOutcomes game =
  List.allPairs [game.dealer] game.players
  |> List.map (fun (dealer,player) -> determineOutcome dealer player)
  


