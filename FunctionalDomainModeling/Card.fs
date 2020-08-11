module Card

open Rank
open Suit

type Card = {rank:Rank; suit:Suit}

let format c = sprintf "%A of %A\n" c.rank c.suit