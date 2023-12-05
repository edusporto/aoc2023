import Aoc2023.Common

namespace Day1

def solve1 (lines : List String) : Int :=
  let n1 := fun s : String => s.get (s.find Char.isDigit)
  let n2 := fun s : String => s.get (s.revFind Char.isDigit).get!
  let getNum := fun s => (String.mk (n1 s :: [n2 s])).toInt!
  let nums := lines.map getNum
  nums.foldl (Int.add) 0

-- ==================================

def startsWith (s1 : String) (s2 : String) : Bool :=
  let (l1, l2) := (s1.toList, s2.toList)
  let zipped := l1.zip l2
  l1.length == zipped.length && zipped.all (fun (x, y) => x == y)

-- #eval startsWith "one" "oned"

partial def findFirst (options : List String) (goal : String): Option String :=
  let rec test (remainingOptions : List String) (which : String) :=
    match remainingOptions with
    | [] => none
    | option :: rest =>
      if startsWith option which
        then some option
        else test rest which

  match goal.toList with
  | [] => none
  | _ :: rest =>
    match test options goal with
    | none => findFirst options (String.mk rest)
    | some result => some result

-- #eval findFirst ["one", "two"] "abctwoone"

def writtenIntoNumber (n : String) : String :=
  match n with
  | "one" => "1"
  | "two" => "2"
  | "three" => "3"
  | "four" => "4"
  | "five" => "5"
  | "six" => "6"
  | "seven" => "7"
  | "eight" => "8"
  | "nine" => "9"
  | _ => n

def getNumber (line : String) : Int :=
  let String.reverse := String.mk ∘ List.reverse ∘ String.toList

  let options :=
    ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
     "1", "2", "3", "4", "5", "6", "7", "8", "9"]
  let optionsRev := options.map String.reverse

  let s1 := writtenIntoNumber (findFirst options line).get!
  let s2 := writtenIntoNumber ∘ String.reverse $
    (findFirst optionsRev (String.reverse line)).get!

  (s1 ++ s2).toInt!

-- #eval getNumber "aoneblatwoz"

def solve2 (lines : List String) : Int :=
  lines
  |> List.map getNumber
  |> List.foldl (Int.add) 0

def main : IO Unit := do
  let contents := (← Utils.readStdioToEnd).splitOn "\n"
  IO.println (solve1 contents)
  IO.println (solve2 contents)

end Day1
