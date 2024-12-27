import Aoc2024.Lib.Utils
import Std.Data.HashSet.Basic
import Batteries.Data.HashMap.Basic

def parseInput (input : String) : Array ℕ :=
  lines input |>.toArray |>.map String.toNat!

def step (seed : ℕ) : ℕ := Id.run do
  let mut seed := seed
  seed := ((seed * 64) ^^^ seed) % 16777216
  seed := ((seed / 32) ^^^ seed) % 16777216
  seed := ((seed * 2048) ^^^ seed) % 16777216
  seed

def simulate (seed steps : ℕ) : ℕ :=
  List.range steps |>.foldl (λ seed _ ↦ step seed) seed

def solve (seeds : Array ℕ) : ℕ :=
  seeds.toList.map (λ seed ↦ simulate seed 2000) |>.sum

def getFrequency (seed : ℕ) : Batteries.HashMap (ℤ × ℤ × ℤ × ℤ) ℕ := Id.run do
  let mut run := List.range 4 |>.map (λ i ↦ simulate seed i) |>.toArray
  let mut result := Batteries.HashMap.empty
  for _ in [0:1997] do
    let new := step run.back!
    let first := run[0]!
    run := run.push new
    run := run.eraseIdx 0

    let d := run.foldl (λ (ndiff, last) cur =>
      ⟨ndiff.push (Int.ofNat (cur % 10) - Int.ofNat (last % 10)), cur⟩
    ) (⟨#[], first⟩ : Array ℤ × ℕ) |>.fst

    let key := (d[0]!, d[1]!, d[2]!, d[3]!)
    if !result.contains key then
      result := result.insert key (new % 10)

  result

def solve2 (seeds : Array ℕ) : ℕ :=
  seeds.toList.map getFrequency |>.foldl (λ acc freq ↦
    freq.toList.foldl (λ acc (k, v) ↦ acc.insert k (v + acc.findD k 0)) acc
  ) (∅ : Batteries.HashMap (ℤ × ℤ × ℤ × ℤ) ℕ) |>.toList |>.map (λ (_, v) ↦ v) |>.foldl Nat.max 0

def main : IO Unit := IO.interact $ λ input ↦
  let seeds := parseInput input
  s!"Part1: {solve seeds}\n" ++
  s!"Part2: {solve2 seeds}\n"
