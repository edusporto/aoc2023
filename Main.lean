import «Aoc2023»

def main (args : List String) : IO UInt32 := do
  match args with
  | "Day1" :: _ => do Day1.main; pure 0
  | _ => do
    IO.println "choose a day please!"
    pure 0
