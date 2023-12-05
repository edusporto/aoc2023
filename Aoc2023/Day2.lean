import Aoc2023.Common
import Mathlib.Data.List.Defs

namespace Day2

inductive Color where
  | Red
  | Green
  | Blue
deriving Repr, Inhabited, BEq

structure Game where
  id : Nat
  turns : List (Nat × Color)
deriving Repr, Inhabited

def parseGame (line : String) : Game :=
  let parsed := line.split
    (fun c =>
      match c with
      | ' ' | ':' | ',' | ';' => true
      | _ => false)
  |> List.filter (fun s => s != "")
  |> Utils.pairs

  let id := parsed.head!.snd.toNat!
  let turns := parsed.tail!.map $
    fun (l, r) => (l.toNat!, match r with
      | "red" => Color.Red
      | "green" => Color.Green
      | "blue" => Color.Blue
      | _ => panic! "unrecognized color")

  { id, turns }

-- #eval parseGame "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"

def validGame (game : Game) : Bool :=
  let validColor :=
    fun color (amount : Nat) =>
      let withThisColor := game.turns
      |> List.filter (·.snd == color)
      withThisColor.all (·.fst <= amount)

  let redValid := validColor Color.Red 12
  let greenValid := validColor Color.Green 13
  let blueValid := validColor Color.Blue 14

  redValid && greenValid && blueValid

-- #eval validGame $ parseGame "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
-- #eval validGame $ parseGame "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
-- #eval validGame $ parseGame "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"

def solve1 (games : List Game) : Int :=
  games
  |> List.filter validGame
  |> List.map (fun g => g.id)
  |> List.sum

-- ==========================

def minCubesPerColor (color : Color) (game : Game) : Nat :=
  game.turns
  |> List.filter (·.snd == color)
  |> List.map (·.fst)
  |> List.maximum?
  |> Option.get!

-- #eval minCubesPerColor Color.Blue $
--   parseGame "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"

def solve2 (games : List Game) : Nat :=
  let minCubes :=
    fun (game : Game) =>
      (minCubesPerColor Color.Red game,
       minCubesPerColor Color.Green game,
       minCubesPerColor Color.Blue game)
  games
  |> List.map minCubes
  |> List.map (fun (x, y, z) => x * y * z)
  |> List.sum

-- #eval solve2 $
--   [parseGame "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
--    parseGame "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
--    parseGame "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
--    parseGame "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
--    parseGame "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"]

def main : IO Unit := do
  let contents := (← Utils.readStdioToEnd).splitOn "\n"
  let games := contents.map parseGame
  IO.println (solve1 games)
  IO.println (solve2 games)

end Day2
