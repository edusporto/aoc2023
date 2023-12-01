namespace Utils

-- def readStdinToEnd : IO String := do

--   IO.Std
--   (← IO.getStdin)

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

end Utils


-- namespace Internal

-- def bufsize : USize := 20 * 1024

-- partial def dump (stream : IO.FS.Stream) : IO Unit := do
--   let buf ← stream.read bufsize
--   if buf.isEmpty then
--     pure ()
--   else
--     (← IO.getStdout).write buf
--     dump stream

-- end Internal
