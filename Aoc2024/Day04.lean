import Aoc2024.Lib.Utils

inductive XMAS where
  | X | M | A | S
deriving Inhabited, BEq

abbrev Map := Array $ Array XMAS

def XMAS.parseChar (c : Char) : XMAS :=
  match c with
  | 'X' => XMAS.X
  | 'M' => XMAS.M
  | 'A' => XMAS.A
  | 'S' => XMAS.S
  | _ => panic! "invalid char"

def parseRow (row : String) : Array XMAS :=
  row.toList.map .parseChar |>.toArray

def parseInput (input : String) : Map :=
  input.splitOn "\n" |>.map parseRow |>.toArray

def directions : List (ℤ × ℤ) :=
  (
    [-1, 0, 1].flatMap $ λ di =>
    [-1, 0, 1].map $ λ dj => (⟨di, dj⟩ : (ℤ × ℤ))
  ).filter (λ dir => dir ≠ ⟨0, 0⟩)

def ZtoN (z : ℤ) : Option ℕ :=
  if z ≥ 0 then some $ z.toNat else none

def getMap (map : Map) (i j : ℤ) : Option XMAS := do
  let i ← ZtoN i
  let j ← ZtoN j
  let row ← map.get? i
  row.get? j

def check (map : Map) (i j : ℤ) (expected : XMAS) : Bool :=
  match getMap map i j with
  | some x => x == expected
  | none => false

def countXMASatDir (map : Map) (i j di dj : ℤ) : Bool :=
  check map (i + 0*di) (j + 0*dj) XMAS.X &&
  check map (i + 1*di) (j + 1*dj) XMAS.M &&
  check map (i + 2*di) (j + 2*dj) XMAS.A &&
  check map (i + 3*di) (j + 3*dj) XMAS.S

def countXMASat (map : Map) (i j : ℤ) : ℕ :=
  directions.filter (uncurry $ countXMASatDir map i j) |>.length

def getCorners (map : Map) (i j : ℤ) : Option (List XMAS) := do
  let a ← getMap map (i - 1) (j - 1)
  let b ← getMap map (i - 1) (j + 1)
  let c ← getMap map (i + 1) (j + 1)
  let d ← getMap map (i + 1) (j - 1)
  some [a, b, c, d]

def isX_MAS (map : Map) (i j : ℤ) : Option Bool := do
  let center ← getMap map i j
  let corners ← getCorners map i j
  some center == XMAS.A && corners.count XMAS.M == 2 && corners.count XMAS.S == 2 && corners.get! 0 != corners.get! 2

def countX_MASat (map : Map) (i j : ℤ) : ℕ :=
  match isX_MAS map i j with
  | some r => r.toNat
  | none => 0

def countXMAS (map : Map) (countAt : Map → ℤ → ℤ → ℕ): ℕ :=
  (
    List.range map.size |>.flatMap $ λ i =>
    List.range (map.get! i |>.size) |>.map $ λ j =>
    countAt map (.ofNat i) (.ofNat j)
  ).foldl .add 0

def main : IO Unit := IO.interact $ λ input ↦
  let map := parseInput input
  s!"Part1: {countXMAS map countXMASat}\n" ++
  s!"Part2: {countXMAS map countX_MASat}\n"
