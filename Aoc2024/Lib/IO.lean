namespace IO

def readInput : Nat → IO String
| 0 => return ""
| T+1 => do
  let stdin ← IO.getStdin
  let line ← stdin.getLine
  if line.length == 0 then
    return ""
  else
    let rest ← readInput T
    return line ++ rest

def interact (f : String → String) : IO Unit := do
  (← readInput 1000000) |> f |> IO.println

end IO
