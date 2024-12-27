import Aoc2024.Lib.Utils
import Std.Data.HashSet.Basic
import Batteries.Data.HashMap.Basic

inductive Kind where
  | Lock : Array ℕ → Kind
  | Key : Array ℕ → Kind


instance : ToString Kind where
  toString
    | Kind.Lock border => s!"Lock {border}"
    | Kind.Key border => s!"Key {border}"

def parseOne (input : String) : Kind := Id.run do
  let mut border := Array.mkArray 5 0

  for line in lines input do
    for (j, c) in line.toList.enum do
      if c == '#' then
        border := border.modify j (λ a => a + 1)

  if (lines input |>.head!) == "#####" then
    Kind.Lock border
  else
    Kind.Key border


def parseInput (input : String) : Array Kind := Id.run do
  input.splitOn "\n\n" |>.map parseOne |>.toArray

def solve (input : Array Kind) : ℕ := Id.run do
  let mut answer := (0 : ℕ)

  let locks := input.filterMap (λ
    | Kind.Lock border => some border
    | _ => none
  )

  let keys := input.filterMap (λ
    | Kind.Key border => some border
    | _ => none
  )

  for lock in locks do
    for key in keys do
      let mut ok := true
      for i in [0:5] do
        if lock[i]! + key[i]! > 7 then
          ok := false
          break
      if ok then
        answer := answer + 1

  answer

def main : IO Unit := IO.interact $ λ input ↦
  let input := parseInput input
  s!"Part1: {solve input}\n" ++
  s!"Part2: {0}\n"
