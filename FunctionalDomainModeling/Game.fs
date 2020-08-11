module Game

open Player
open Dealer
open Deck
open MaybeBuilder

type Game = {players:Player list; dealer:Dealer; deck:Deck}

let create numPlayers deck =
  maybe{
    let! players,deck = Player.createMultiple numPlayers deck
    let! dealer,deck = Dealer.create deck
    return {players=players; dealer=dealer; deck=deck}
  }