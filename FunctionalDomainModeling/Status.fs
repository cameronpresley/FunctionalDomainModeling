module Status

open Points
open Action 

type Status = CardsDealt | Busted of int | Stayed of int | Blackjack
