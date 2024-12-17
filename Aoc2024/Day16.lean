import Aoc2024.Lib.Utils
import Std.Data.HashSet.Basic
import Batteries.Data.HashMap.Basic

def parseInput (input : String) : ℕ := sorry

def main : IO Unit := IO.interact $ λ input ↦
  let input := parseInput input
  s!"Part1: {0}\n" ++
  s!"Part2: {0}\n"
