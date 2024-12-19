import Aoc2024.Lib.Utils
import Std.Data.HashSet.Basic
import Batteries.Data.HashMap.Basic

def parseInput (input : String) : List String × List String :=
  let (towels, pattern) := input.splitOn "\n\n" |>.first2!
  let towels := towels.splitOn ", "
  let pattern := lines pattern
  (towels, pattern)

def solveOne (towels : List String) (pattern : String) : ℕ := Id.run do
  let mut dp := Array.mkArray (pattern.length + 1) (0 : ℕ)
  dp := dp.modify 0 (λ _ => 1)

  for i in [1:pattern.length + 1] do
    for towel in towels do
      if i < towel.length then
        continue
      let sub := pattern.drop (i - towel.length) |>.take towel.length
      if sub == towel then
        dp := dp.modify i (λ v => v + dp[i - towel.length]!)

  return dp[pattern.length]!

def solve (towels patterns : List String) : ℕ :=
  patterns.filter (λ p => solveOne towels p > 0) |>.length

def solve1 (towels patterns : List String) : ℕ :=
  patterns.map (λ p => solveOne towels p) |>.sum

def main : IO Unit := IO.interact $ λ input ↦
  let (towels, pattern) := parseInput input
  s!"Part1: {solve towels pattern}\n" ++
  s!"Part2: {solve1 towels pattern}\n"
