import Std.Data.HashSet.Basic

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

def interactM (f : String → IO String) : IO Unit := do
  let input ← readInput 1000000
  let output ← f input
  IO.println output

end IO

def absDiff (a b : ℕ) : ℕ :=
  (a - b) + (b - a)

def List.first2! [Inhabited α] : List α → α × α
| x :: y :: _ => (x, y)
| _ => panic "List.first2!: list does not contain 2 elements"

def List.first3! [Inhabited α] : List α → α × α × α
| x :: y :: z :: _ => (x, y, z)
| _ => panic "List.first3!: list does not contain 3 elements"

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

def DigitToNat (c : Char) : Option ℕ :=
  if '0' ≤ c ∧ c ≤ '9' then
    some $ Char.toNat c - Char.toNat '0'
  else
    none

def DigitToNat! (c : Char) : ℕ :=
  match DigitToNat c with
  | some x => x
  | none => panic! "DigitToNat!: invalid digit"

def gridPositions (n m : ℕ) : List (ℕ × ℕ) :=
  List.range n |>.flatMap (λ i ↦ List.range m |>.map (λ j ↦ (i, j)))

def getDigit? (c : Char) : Option ℕ :=
  if c.isDigit then
    some (c.toNat - '0'.toNat)
  else
    none

def extractNumbers (line : String) : List ℕ :=
  let (l, d) := line.toList.foldl (λ (l, v) c =>
    match v, getDigit? c with
    | some v, some d => (l, some (v * 10 + d))
    | some v, none => (v :: l, none)
    | none, d => (l, d)
  ) (⟨[], none⟩ : List ℕ × Option ℕ)

  let l := match d with
  | some d => d :: l
  | none => l

  l.reverse

def List.toHashSet {α : Type} [BEq α] [Hashable α] (l : List α) : Std.HashSet α :=
  l.foldl (λ set x => set.insert x) ∅

def Int.max (a b : Int) : Int := if a > b then a else b
