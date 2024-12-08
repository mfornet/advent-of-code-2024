import Aoc2024.Lib.Utils
import Std.Data.HashSet.Basic
import Batteries.Data.HashMap.Basic

structure Line where
  target : ℕ
  values : List ℕ
deriving Repr

def Line.parse (input : String) : Line :=
  let ⟨target, values⟩ := input.splitOn ": " |>.first2!
  let target := target.toNat!
  let values := values.splitOn " " |>.map String.toNat!
  Line.mk target values

def canAchieveRec (target current : ℕ) (values : List ℕ) (operators : ℕ → ℕ → List ℕ) : Bool :=
  if current > target then
    false
  else
    match values with
    | [] => current == target
    | x :: xs =>
      operators current x |>.any (canAchieveRec target · xs operators)

def Line.canAchieveTarget (line : Line) (operators : ℕ → ℕ → List ℕ) : Bool :=
  match line.values with
  | [] => false
  | x :: xs => canAchieveRec line.target x xs operators

def parseInput (input : String) : List Line :=
  lines input |>.map Line.parse

def solve (input : List Line) (operators : ℕ → ℕ → List ℕ) : ℕ :=
  input.filter (Line.canAchieveTarget · operators) |>.map (·.target) |>.sum

partial def NatToStringRec (n : ℕ) : String :=
  match n with
  | 0 => ""
  | x => NatToStringRec (x / 10) ++ (String.singleton (Char.ofNat ('0'.toNat + x % 10)))

def NatToString (n : ℕ) : String :=
  match n with
  | 0 => "0"
  | x => NatToStringRec x

def part1 : ℕ → ℕ → List ℕ
  | x, y => [x + y, x * y]

def part2 : ℕ → ℕ → List ℕ
  | x, y => [x + y, x * y, NatToString x ++ NatToString y |>.toNat!]

def main : IO Unit := IO.interact $ λ input ↦
  let input := parseInput input
  s!"Part1: {solve input part1}\n" ++
  s!"Part2: {solve input part2}\n"
