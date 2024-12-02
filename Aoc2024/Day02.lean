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
  { s with
    last := x,
    safe := s.safe && validLevelDifference s.last x && s.direction == slopeDirection s.last x }

def isSafe : List ℕ → Bool
  | x :: y :: xs => xs.foldl .step (createState x y) |>.safe
  | _ => true

def rec (pref : List ℕ) : List ℕ → Bool
  | [] => false
  | x :: xs => isSafe (pref ++ xs) || rec (pref ++ [x]) xs

def isSafe2 (report : List ℕ) : Bool :=
  (isSafe report) || rec [] report

def solve (filter : List ℕ → Bool) (reports : List (List ℕ)) : ℕ :=
  reports.filter filter |>.length

def main : IO Unit := IO.interact $ λ input ↦
  let input := lines input |>.map parseLine
  s!"Part1: {solve isSafe input}\n" ++
  s!"Part2: {solve isSafe2 input}\n"
