import Aoc2024.Lib.Utils
import Std.Data.HashSet.Basic
import Batteries.Data.HashMap.Basic

structure Program where
  a : ℕ
  b : ℕ
  c : ℕ
  program : Array ℕ
deriving Inhabited

instance : ToString Program where
  toString p := s!"{p.a} {p.b} {p.c} {p.program}"

def parseInput (input : String) : Program :=
  let (first, second) := input.splitOn "\n\n" |>.first2!
  let (a, b, c) := lines first |>.map (λ l => l.splitOn ": " |>.get! 1 |>.toNat!) |>.first3!
  let program := second.stripSuffix "\n" |>.splitOn ": " |>.get! 1 |>.splitOn "," |>.map (λ l => l.toNat!)
  Program.mk a b c program.toArray

structure Simulator where
  program : Program
  pc : ℕ
  output : Array ℕ
deriving Inhabited

def Simulator.combo (s : Simulator) (c : ℕ) : ℕ :=
  match c with
  | 0 | 1 | 2 | 3 => c
  | 4 => s.program.a
  | 5 => s.program.b
  | 6 => s.program.c
  | _ => panic! "invalid"

def Simulator.step (s : Simulator) : Bool × Simulator := Id.run do
  if s.pc >= s.program.program.size - 1 then
    return ⟨true, s⟩

  let instruction := s.program.program.get! s.pc
  let operand := s.program.program.get! (s.pc + 1)
  let s := { s with pc := s.pc + 2 }

  ⟨false,
  match instruction with
  | 0 =>
    let newA := s.program.a.shiftRight (s.combo operand)
    { s with program := { s.program with a := newA } }
  | 1 =>
    let newB := Nat.xor s.program.b operand
    { s with program := { s.program with b := newB } }
  | 2 =>
    let newB := (s.combo operand) % 8
    { s with program := { s.program with b := newB } }
  | 3 =>
    if s.program.a == 0 then
      s
    else
      { s with pc := operand }
  | 4 =>
    let newB := Nat.xor s.program.b s.program.c
    { s with program := { s.program with b := newB } }
  | 5 =>
    let newOut := (s.combo operand) % 8
    { s with output := s.output.push newOut }
  | 6 =>
    let newB := s.program.a.shiftRight (s.combo operand)
    { s with program := { s.program with b := newB } }
  | 7 =>
    let newC := s.program.a.shiftRight (s.combo operand)
    { s with program := { s.program with c := newC } }
  | _ => panic! "invalid"
  ⟩

def Simulator.getOutput (simulator : Simulator) : String :=
  String.intercalate "," $ simulator.output.toList.map toString

def solve (program : Program) : String := Id.run do
  let mut simulator := Simulator.mk program 0 #[]

  while true do
    let (done, s0) := simulator.step
    simulator := s0

    if done then
      break

  simulator.getOutput


def eval (a : ℕ) : ℕ := Id.run do
  let mut a := a
  let mut out := 0
  while a > 0 do
    let b := Nat.xor (a % 8) 3
    let c := a.shiftRight b
    let b := Nat.xor (Nat.xor b c) 5
    a := a.shiftRight 3
    out := out * 8 + b % 8
  return out

def backtrack (s t m n : ℕ) : Option ℕ :=
  match m with
  | 0 => s
  | m' + 1 => Id.run do
    for i in [0:8] do
      let seq := s * (8 ^ m) + i * (8 ^ m')
      let out := eval seq
      let mask := 8 ^ (n + 1) - 1

      if (t &&& mask) == (out &&& mask) then
        match backtrack (s * 8 + i) t m' (n + 1) with
        | some res => return some res
        | none => continue
    none

def solve2 (program : Program) : ℕ :=
  let t := program.program.foldl (λ acc x ↦ acc * 8 + x) 0
  backtrack 0 t program.program.size 0 |>.get!

def main : IO Unit := IO.interact $ λ input ↦
  let program := parseInput input
  s!"Part1: {solve program}\n" ++
  s!"Part2: {solve2 program}\n"
