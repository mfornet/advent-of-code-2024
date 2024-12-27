import Aoc2024.Lib.Utils
import Std.Data.HashSet.Basic
import Batteries.Data.HashMap.Basic

inductive Gate where
  | And : ℕ → ℕ → ℕ → Gate
  | Or : ℕ → ℕ → ℕ → Gate
  | Xor : ℕ → ℕ → ℕ → Gate

structure System where
  target : Array ℕ
  initial : Array ℕ
  gates : Array Gate

def parseInput (input : String) : System := Id.run do
  let (head, tail) := input.splitOn "\n\n" |>.first2!

  let mut rev : Batteries.HashMap String ℕ := ∅

  for line in lines head do
    let name := line.splitOn ":" |>.head!
    if !rev.contains name then
      rev := rev.insert name rev.size

  for line in lines tail do
    let tokens := line.splitOn " "
    let names := [tokens[0]!, tokens[2]!, tokens[4]!]
    for name in names do
      if !rev.contains name then
        rev := rev.insert name rev.size

  let mut initial := Array.mkArray rev.size 2
  let mut gates := (#[] : Array Gate)

  for line in lines head do
    let (name, value) := line.splitOn ": " |>.first2!
    let value := value.toNat!
    let index := rev.find! name
    initial := initial.set! index value

  for line in lines tail do
    let tokens := line.splitOn " "
    let names := [tokens[0]!, tokens[2]!, tokens[4]!] |>.map (λ name ↦ rev.find! name)
    match tokens[1]! with
    | "AND" => gates := gates.push (Gate.And names[0]! names[1]! names[2]!)
    | "OR" => gates := gates.push (Gate.Or names[0]! names[1]! names[2]!)
    | "XOR" => gates := gates.push (Gate.Xor names[0]! names[1]! names[2]!)
    | _ => panic! "impossible"

  let target := rev.toList.filterMap (λ (name, _) ↦
    if name.startsWith "z" then
      some name
    else
      none
  ) |>.toArray |>.qsort (λ a b ↦ a > b) |>.map rev.find!

  System.mk target initial gates

def solve (system : System) : ℕ := Id.run do
  let mut change := true
  let mut v := system.initial
  while change do
    change := false
    for gate in system.gates do
      match gate with
      | Gate.And i0 i1 i2 =>
        if v[i0]! != 2 && v[i1]! != 2 && v[i2]! == 2 then
          v := v.set! i2 (v[i0]! &&& v[i1]!)
          change := true
      | Gate.Or i0 i1 i2 =>
        if v[i0]! != 2 && v[i1]! != 2 && v[i2]! == 2 then
          v := v.set! i2 (v[i0]! ||| v[i1]!)
          change := true
      | Gate.Xor i0 i1 i2 =>
        if v[i0]! != 2 && v[i1]! != 2 && v[i2]! == 2 then
          v := v.set! i2 (v[i0]! ^^^ v[i1]!)
          change := true

  let mut answer := 0
  for u in system.target do
    answer := answer * 2 + v[u]!
  answer

def main : IO Unit := IO.interact $ λ input ↦
  let system := parseInput input
  s!"Part1: {solve system}\n" ++
  s!"Part2: {0}\n"
