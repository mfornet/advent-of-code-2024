import Aoc2024.Lib.Utils

def parseLine (line : String) : List Nat :=
  line.splitOn " " |>.map String.toNat!

inductive Slope
  | Increasing
  | Decreasing
deriving BEq

structure State where
  direction : Slope
  last : Nat
  safe: Bool

def validLevelDifference (x₀ x₁ : Nat) : Bool :=
  let diff := absDiff x₀ x₁
  1 <= diff ∧ diff ≤ 3

def slopeDirection (x₀ x₁ : Nat) : Slope :=
  if x₀ < x₁ then Slope.Increasing else Slope.Decreasing

def createState (x₀ x₁ : Nat) : State :=
  { direction := slopeDirection x₀ x₁, last := x₁, safe := validLevelDifference x₀ x₁ }

def State.step (s : State) (x : Nat) : State :=
  { direction := slopeDirection s.last x,
    last := x,
    safe := s.safe && validLevelDifference s.last x && s.direction == slopeDirection s.last x }

def isSafe (report : List Nat) : Bool := match report with
  | [] => true
  | _ :: [] => true
  | x :: y :: xs => xs.foldl (λ s x => s.step x) (createState x y) |>.safe

def rec (pref suff : List Nat) : Bool :=
  match suff with
  | [] => false
  | x :: xs => isSafe (pref ++ xs) || rec (pref ++ [x]) xs

def isSafe2 (report : List Nat) : Bool :=
  (isSafe report) || rec [] report

def solve (filter : List Nat → Bool) (reports : List (List Nat)) : Nat :=
  reports.filter filter |>.length

def main : IO Unit := IO.interact $ λ input =>
  let input := lines input |>.map parseLine
  s!"Part1: {solve isSafe input}\n" ++
  s!"Part2: {solve isSafe2 input}\n"
