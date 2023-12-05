namespace Utils

partial def readStreamToEnd (h : IO.FS.Stream) : IO String := do
  let rec loop (s : String) := do
    let line ← h.getLine
    if line.isEmpty then
      return s
    else
      loop (s ++ line)
  loop ""

def readStdioToEnd : IO String := do
  readStreamToEnd (<- IO.getStdin)

def pairs {α : Type} (l : List α) : List (α × α) :=
  match l with
  | [] => []
  | _ :: [] => []
  | x :: y :: rest => (x, y) :: pairs rest

-- #eval pairs [1, 2, 3, 4]

-- class Zero.{u} (α : Type u) where
--   zero : α

-- instance Zero.toOfNat0 {α} [Zero α] : OfNat α (nat_lit 0) where
--   ofNat := ‹Zero α›.1

-- instance Zero.ofOfNat0 {α} [OfNat α (nat_lit 0)] : Zero α where
--   zero := 0

-- def List.sum [Add α] [Zero α] : List α → α :=
--   List.foldl (· + ·) 0

end Utils
