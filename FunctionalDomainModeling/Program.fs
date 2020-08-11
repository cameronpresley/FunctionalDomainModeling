// Learn more about F# at http://fsharp.org

open System
open Game
open MaybeBuilder
open Outcome
open Turn
[<EntryPoint>]
let main argv =
    let convertOutcomeToMessage status =
        match status with
        | PlayerBlackjack -> "Player won with a blackjack!"
        | DealerBlackjack -> "Dealer won with a blackjack!"
        | PlayerWon -> "Player beat the dealer!"
        | DealerWon -> "Dealer beat the player!"
        | Push -> "It's a tie!"
    let rand = System.Random()
    let shuffler _ = rand.Next(1000)
    let deck = Deck.shuffle shuffler

    let outcomesToMessages = (List.map convertOutcomeToMessage) >> (List.reduce (+))
    // let value = 
    deck 
        |> Game.create 1
        |> Option.bind (Turn.playersTakeTurn Turn.promptPlayer)
        |> Option.bind Turn.dealerTakesTurn
        |> Option.map Outcome.determineOutcomes
        |> Option.map outcomesToMessages
        |> Option.iter (printfn "%s" )
    //printfn "%A" value
    //|> Option.iter printfn "%s"
    0 // return an integer exit code
