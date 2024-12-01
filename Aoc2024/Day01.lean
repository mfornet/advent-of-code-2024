import Aoc2024.Lib.IO
import Aoc2024.Lib.Aoc
import Aoc2024.Lib.List
import Batteries.Data.HashMap.Basic

def parseLine (line : String) : Nat × Nat :=
  line.splitOn "   " |>.map String.toNat! |>.first2!

def absDiff (a b : Nat) : Nat :=
  if a > b then a - b else b - a

def frequency (l : List Nat) : Batteries.HashMap Nat Nat :=
  l.foldl (λ m n => m.insert n (m.findD n 0 + 1)) Batteries.HashMap.empty

def part1 (l₀ l₁ : List Nat) : Nat :=
  List.zip l₀ l₁ |>.map (λ ⟨a, b⟩ => absDiff a b) |>.foldl Nat.add 0

def part2 (l₀ l₁ : List Nat) : Nat :=
  let l₁ := frequency l₁
  l₀.map (λ n => n * l₁.findD n 0) |>.foldl Nat.add 0

def main : IO Unit := IO.interact $ λ input =>
  let ⟨l₀, l₁⟩ := lines input
    |>.map parseLine
    |>.unzip
    |>.tolist
    |>.map List.mergeSort
    |>.first2!

  s!"Part1: {part1 l₀ l₁}\n" ++
  s!"Part2: {part2 l₀ l₁}\n"
