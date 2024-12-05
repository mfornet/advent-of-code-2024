abbrev ℕ := Nat
abbrev ℤ := Int

def lines (s : String) : List String := s.splitOn "\n" |>.reverse |>.dropWhile String.isEmpty |>.reverse

def Prod.tolist {α} (p : α × α) : List α := [p.1, p.2]

def uncurry (f : α → β → γ) : (α × β → γ) | (a, b) => f a b

namespace IO

def readInput : ℕ → IO String
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

def absDiff (a b : ℕ) : ℕ :=
  (a - b) + (b - a)

def List.first2! [Inhabited α] : List α → α × α
| x :: y :: _ => (x, y)
| _ => panic "List.first2!: list does not contain 2 elements"

structure Debug (α : Type) where
  value : α
  logs : List String

instance : Monad (Debug) where
  bind x f :=
    let ⟨value, logs⟩ := f x.value
    ⟨value, x.logs ++ logs⟩
  pure x := ⟨x, []⟩

def debug (log : String) : Debug Unit :=
  Debug.mk () [log]

def ok (value : α) : Debug α :=
  Debug.mk value []
