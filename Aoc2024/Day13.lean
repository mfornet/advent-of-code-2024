import Aoc2024.Lib.Utils
import Std.Data.HashSet.Basic
import Batteries.Data.HashMap.Basic

abbrev PairN := ℕ × ℕ

instance : Inhabited PairN := ⟨(0, 0)⟩

structure Machine where
  buttonA : PairN
  buttonB : PairN
  prize : PairN
deriving Inhabited

instance : ToString Machine where
  toString m := s!"{m.buttonA} {m.buttonB} {m.prize}"

def getDigit? (c : Char) : Option ℕ :=
  if c.isDigit then
    some (c.toNat - '0'.toNat)
  else
    none

def extractNumbers (line : String) : List ℕ :=
  let (l, d) := line.toList.foldl (λ (l, v) c =>
    match v, getDigit? c with
    | some v, some d => (l, some (v * 10 + d))
    | some v, none => (v :: l, none)
    | none, d => (l, d)
  ) (⟨[], none⟩ : List ℕ × Option ℕ)

  let l := match d with
  | some d => d :: l
  | none => l

  l.reverse

def parseMachine (input : String) : Machine :=
  let (a, b, p) := lines input |>.map extractNumbers |>.map (λ l => l.first2!) |>.first3!
  Machine.mk a b p

def parseInput (input : String) : List Machine :=
  input.splitOn "\n\n" |>.map parseMachine

def Machine.find? (m : Machine) : IO (Option ℕ) := do
  let a := Int.ofNat m.buttonA.1
  let b := Int.ofNat m.buttonB.1
  let u := Int.ofNat m.prize.1
  let c := Int.ofNat m.buttonA.2
  let d := Int.ofNat m.buttonB.2
  let v := Int.ofNat m.prize.2

  if u % b == 0 && v % d == 0 && u / b == v / d then
    return some (u / b).toNat
  else
    let det := a * d - b * c
    if det == 0 then
      return none
    else
      let san := d * u - b * v
      let sbn := -c * u + a * v

      if san * det >= 0 && san % det.natAbs == 0 && sbn * det >= 0 && sbn % det.natAbs == 0 then
        let san := san / det
        let sbn := sbn / det
        return some (san * 3 + sbn).toNat
      else
        return none

def Machine.adjustPrice (m : Machine) (extra : ℕ) : Machine :=
  Machine.mk (m.buttonA) (m.buttonB) (m.prize.1 + extra, m.prize.2 + extra)

def solve (machines : List Machine) (extra : ℕ ): IO ℕ := do
  let ls ← machines.map (·.adjustPrice extra) |>.filterMapM (λ m => m.find?)
  return ls.sum

def main : IO Unit := IO.interactM $ λ input ↦ do
  let machines := parseInput input
  return s!"Part1: {← solve machines 0}\n" ++
  s!"Part2: {← solve machines 10000000000000}\n"
