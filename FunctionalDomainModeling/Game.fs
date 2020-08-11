module Game

open Player
open Dealer
open Deck
open MaybeBuilder

type Game =
    { Players: Player list
      Dealer: Dealer
      Deck: Deck }

let create numPlayers deck =
    maybe {
        let! players, deck = Player.createMultiple numPlayers deck
        let! dealer, deck = Dealer.create deck

        return { Players = players
                 Dealer = dealer
                 Deck = deck }
    }
