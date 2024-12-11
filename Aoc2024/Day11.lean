import Aoc2024.Lib.Utils
import Std.Data.HashSet.Basic
import Batteries.Data.HashMap.Basic

def parseInput (input : String) : List ℕ :=
  input.stripSuffix "\n" |>.splitOn " " |>.map String.toNat!

abbrev Cache := Batteries.HashMap (ℕ × ℕ) ℕ

partial def NatToDigitsLoop (n : ℕ) (acc : Array ℕ) : Array ℕ :=
  if n == 0 then acc
  else NatToDigitsLoop (n / 10) (acc.push (n % 10))

def NatToDigits (n : ℕ) : Array ℕ :=
  match n with
  | 0 => #[0]
  | _ => NatToDigitsLoop n #[] |>.reverse

def DigitsToNat (digits : Array ℕ) : ℕ :=
  digits.foldl (λ acc d => acc * 10 + d) 0

def countAfter (value : ℕ) (steps : ℕ) (cache : Cache) : ℕ × Cache :=
  match steps with
  | 0 => (1, cache)
  | s + 1 =>
    match cache.find? (value, s + 1) with
    | some x => (x, cache)
    | none =>
      if value == 0 then
        let (count, cache) := countAfter 1 s cache

        let cache := cache.insert (value, s + 1) count
        (count, cache)
      else
        let digits := NatToDigits value
        if digits.size % 2 == 0 then
          let left := DigitsToNat $ digits.extract 0 (digits.size / 2)
          let right := DigitsToNat $ digits.extract (digits.size / 2) digits.size

          let (leftCount, cache) := countAfter left s cache
          let (rightCount, cache) := countAfter right s cache
          let count := leftCount + rightCount

          let cache := cache.insert (value, s + 1) count
          (count, cache)
        else
          let (count, cache) := countAfter (value * 2024) s cache
          let cache := cache.insert (value, s + 1) count
          (count, cache)

def solve (stones : List ℕ) (steps : ℕ) : ℕ :=
  stones.foldl (λ (answer, cache) value =>
    let (cur, cache) := countAfter value steps cache
    (answer + cur, cache)
  ) (0, ∅) |>.fst


def main : IO Unit := IO.interact $ λ input ↦
  let stones := parseInput input
  s!"Part1: {solve stones 25}\n" ++
  s!"Part2: {solve stones 75}\n"
