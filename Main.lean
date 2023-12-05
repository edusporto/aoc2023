import «Aoc2023»

def main (args : List String) : IO UInt32 := do
  match args with
  | "Day1" :: _ => Day1.main
  | "Day2" :: _ => Day2.main
  | _ => IO.println "choose a day please!"
  pure 0
