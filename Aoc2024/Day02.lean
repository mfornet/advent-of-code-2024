import Aoc2024.Lib.Utils

def parseLine (line : String) : List ℕ :=
  line.splitOn " " |>.map String.toNat!

inductive Slope
  | Increasing
  | Decreasing
deriving BEq

structure State where
  direction : Slope
  last : ℕ
  safe: Bool

def validLevelDifference (x₀ x₁ : ℕ) : Bool :=
  let diff := absDiff x₀ x₁
  1 <= diff ∧ diff ≤ 3

def slopeDirection (x₀ x₁ : ℕ) : Slope :=
  if x₀ < x₁ then Slope.Increasing else Slope.Decreasing

def createState (x₀ x₁ : ℕ) : State :=
  { direction := slopeDirection x₀ x₁, last := x₁, safe := validLevelDifference x₀ x₁ }

def State.step (s : State) (x : ℕ) : State :=
  { direction := slopeDirection s.last x,
    last := x,
    safe := s.safe && validLevelDifference s.last x && s.direction == slopeDirection s.last x }

def isSafe (report : List ℕ) : Bool := match report with
  | [] => true
  | _ :: [] => true
  | x :: y :: xs => xs.foldl (λ s x => s.step x) (createState x y) |>.safe

def rec (pref suff : List ℕ) : Bool :=
  match suff with
  | [] => false
  | x :: xs => isSafe (pref ++ xs) || rec (pref ++ [x]) xs

def isSafe2 (report : List ℕ) : Bool :=
  (isSafe report) || rec [] report

def solve (filter : List ℕ → Bool) (reports : List (List ℕ)) : ℕ :=
  reports.filter filter |>.length

def main : IO Unit := IO.interact $ λ input =>
  let input := lines input |>.map parseLine
  s!"Part1: {solve isSafe input}\n" ++
  s!"Part2: {solve isSafe2 input}\n"
