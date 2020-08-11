module Points
open Card
open Rank

type Points = Hard of int | Soft of int*int

let cardToPoint c =
  match c.rank with
  | Ace -> Soft (1, 11)
  | Two -> Hard 2
  | Three -> Hard 3
  | Four -> Hard 4
  | Five -> Hard 5
  | Six -> Hard 6
  | Seven -> Hard 7
  | Eight -> Hard 8
  | Nine -> Hard 9
  | Ten | Jack | Queen | King -> Hard 10

let add a b =
  match (a, b) with
  | Hard x, Hard y -> Hard(x+y)
  | Hard x, Soft(y,z) | Soft(y,z), Hard x -> Soft(x+y, x+z)
  | Soft(x, y), Soft(z, _) -> Soft(x+z, y+z)

let calculateHand =
  List.map cardToPoint >> List.fold add (Hard 0)