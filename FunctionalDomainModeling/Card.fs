module Card

type Rank = Ace  | Two | Three | Four 
          | Five | Six | Seven | Eight 
          | Nine | Ten | Jack  | Queen | King

type Suit = Hearts | Clubs | Spades | Diamonds

type Card = { Rank: Rank; Suit: Suit }

let format c = sprintf "%A of %A\n" c.Rank c.Suit
