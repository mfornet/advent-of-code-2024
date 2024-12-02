import Aoc2024.Lib.Utils
import Batteries.Data.HashMap.Basic

def parseLine (line : String) : ℕ × ℕ :=
  line.splitOn "   " |>.map String.toNat! |>.first2!

def frequency (l : List ℕ) : Batteries.HashMap ℕ ℕ :=
  l.foldl (λ m n => m.insert n (m.findD n 0 + 1)) Batteries.HashMap.empty

def part1 (l₀ l₁ : List ℕ) : ℕ :=
  List.zip l₀ l₁ |>.map (uncurry absDiff) |>.foldl .add 0

def part2 (l₀ l₁ : List ℕ) : ℕ :=
  let l₁ := frequency l₁
  l₀.map (λ n => n * l₁.findD n 0) |>.foldl .add 0

def main : IO Unit := IO.interact $ λ input =>
  let ⟨l₀, l₁⟩ := lines input
    |>.map parseLine
    |>.unzip
    |>.tolist
    |>.map List.mergeSort
    |>.first2!

  s!"Part1: {part1 l₀ l₁}\n" ++
  s!"Part2: {part2 l₀ l₁}\n"
