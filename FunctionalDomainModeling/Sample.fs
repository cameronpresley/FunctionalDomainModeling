module Sample
open MaybeBuilder

type Rank = Ace | Two | Three | Four | Five
          | Six | Seven | Eight | Nine | Ten
          | Jack | Queen | King
type Suit = Hearts | Clubs | Spades | Diamonds
type Card = {Rank:Rank; Suit:Suit}
type Deck = Deck of Card list
type Hand = Card list
type Action = Hit | Stay
type Status = Busted | Stayed | Blackjack
type Points = Hard of int | Soft of int*int
type Player = {Id: int; Hand:Hand;}
type Dealer = {Hand:Hand;}
type Game = {Deck:Deck; Players:Player list; Dealer:Dealer}

let createDeck () =
  let ranks = [Ace; Two; Three; Four; Five; Six; Seven;
               Eight; Nine; Ten; Jack; Queen; King]
  let suits = [Hearts; Clubs; Spades; Diamonds]
  let tupleToCard (r,s) = {Rank=r; Suit=s}

  List.allPairs ranks suits
  |> List.map tupleToCard 
  |> Deck

let draw (Deck d) =
  match d with
  | [] -> None
  | card::restOfDeck -> Some(card, (Deck)restOfDeck)

let createPlayer deck =
  match draw deck with
  | None -> None
  | Some(card, restOfDeck) ->
    match draw restOfDeck with
    | None -> None
    | Some (card', restOfDeck) -> ({Id=1; Hand=[card;card']},restOfDeck) |> Some

let createPlayer' deck = 
  maybe {
    let! card,restOfDeck = draw deck
    let! card',restOfDeck = draw restOfDeck
    return ({Id=1; Hand=[card;card']},restOfDeck)
  }