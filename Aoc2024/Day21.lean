import Aoc2024.Lib.Utils
import Std.Data.HashSet.Basic
import Batteries.Data.HashMap.Basic

def baseKeyPad (dim : ℕ): Array (Array ℕ) := Array.mkArray dim (Array.mkArray dim 0)

def inf : ℕ := 1000000000000000000

def distTable (dim0 dim1 : ℕ) : Array $ Array $ Array $ Array ℕ := Array.mkArray dim0 $ Array.mkArray dim1 $ Array.mkArray dim0 $ Array.mkArray dim1 inf

def update (dist : Array $ Array $ Array $ Array ℕ) (i j k l : ℕ) (d : ℕ) : Array $ Array $ Array $ Array ℕ :=
  dist.modify i (λ a => a.modify j (λ a => a.modify k (λ a => a.modify l (Nat.min d))))

inductive Key where
| Up
| Action
| Left
| Down
| Right
deriving Inhabited

instance (n : ℕ): OfNat Key n where
  ofNat := match n with
    | 0 => Key.Up
    | 1 => Key.Action
    | 2 => Key.Left
    | 3 => Key.Down
    | 4 => Key.Right
    | _ => panic! "impossible"

def Key.toNat (key : Key) : ℕ :=
  match key with
  | Key.Up => 0
  | Key.Action => 1
  | Key.Left => 2
  | Key.Down => 3
  | Key.Right => 4

def Key.position (key : Key) : ℤ × ℤ :=
  match key with
  | Key.Up => ⟨0, 1⟩
  | Key.Action => ⟨0, 2⟩
  | Key.Left => ⟨1, 0⟩
  | Key.Down => ⟨1, 1⟩
  | Key.Right => ⟨1, 2⟩

def Key.delta (key : Key) : ℤ × ℤ :=
  match key with
  | Key.Up => ⟨-1, 0⟩
  | Key.Action => ⟨0, 0⟩
  | Key.Left => ⟨0, -1⟩
  | Key.Down => ⟨1, 0⟩
  | Key.Right => ⟨0, 1⟩

def fromPosition (pos : ℤ × ℤ) : Option Key :=
  match pos with
  | ⟨0, 1⟩ => some Key.Up
  | ⟨0, 2⟩ => some Key.Action
  | ⟨1, 0⟩ => some Key.Left
  | ⟨1, 1⟩ => some Key.Down
  | ⟨1, 2⟩ => some Key.Right
  | _ => none

def digitPosition (digit : ℕ) : ℤ × ℤ :=
  match digit with
  | 0 => ⟨3, 1⟩
  | 1 => ⟨2, 0⟩
  | 2 => ⟨2, 1⟩
  | 3 => ⟨2, 2⟩
  | 4 => ⟨1, 0⟩
  | 5 => ⟨1, 1⟩
  | 6 => ⟨1, 2⟩
  | 7 => ⟨0, 0⟩
  | 8 => ⟨0, 1⟩
  | 9 => ⟨0, 2⟩
  -- Action
  | 10 => ⟨3, 2⟩
  | _ => panic! "impossible"

def digitFromPosition (pos : ℤ × ℤ) : Option ℕ :=
  match pos with
  | ⟨3, 1⟩ => some 0
  | ⟨2, 0⟩ => some 1
  | ⟨2, 1⟩ => some 2
  | ⟨2, 2⟩ => some 3
  | ⟨1, 0⟩ => some 4
  | ⟨1, 1⟩ => some 5
  | ⟨1, 2⟩ => some 6
  | ⟨0, 0⟩ => some 7
  | ⟨0, 1⟩ => some 8
  | ⟨0, 2⟩ => some 9
  | ⟨3, 2⟩ => some 10
  | _ => none

def nextKeyPad (base : Array (Array ℕ)) : Array (Array ℕ) := Id.run do
  -- 5: ^ A < v >
  let mut dist := distTable 5 5

  for i0 in [0:5] do
    for i1 in [0:5] do
      -- Self
      dist := update dist i0 i1 i0 i1 0

      -- Press action key
      let k0 : Key := OfNat.ofNat i0
      let k1 : Key := OfNat.ofNat i1
      let (px, py) := k0.position
      let (dx, dy) := k1.delta
      let (nx, ny) := (px + dx, py + dy)
      let nk := fromPosition ⟨nx, ny⟩

      match nk with
      | some nk => dist := update dist i0 i1 nk.toNat i1 1
      | none => ()

      for i2 in [0:5] do
        dist := update dist i0 i1 i0 i2 (base[i1]![i2]!)

  -- floyd warshall
  for i0 in [0:5] do
    for i1 in [0:5] do
      for i2 in [0:5] do
        for i3 in [0:5] do
          for i4 in [0:5] do
            for i5 in [0:5] do
              dist := update dist i2 i3 i4 i5 (dist[i2]![i3]![i0]![i1]! + dist[i0]![i1]![i4]![i5]!)

  -- Compute new base
  let mut result := baseKeyPad 5
  for i in [0:5] do
    for j in [0:5] do
      result := result.modify i (λ row => row.set! j dist[i]![1]![j]![1]!)
  return result

def mainKeyPad (base : Array (Array ℕ)) : Array (Array ℕ) := Id.run do
  let mut dist := distTable 11 5

  for i0 in [0:11] do
    for i1 in [0:5] do
      -- Self
      dist := update dist i0 i1 i0 i1 0

      -- Press action key
      let k1 : Key := OfNat.ofNat i1
      let (px, py) := digitPosition i0
      let (dx, dy) := k1.delta
      let (nx, ny) := (px + dx, py + dy)
      let nd := digitFromPosition ⟨nx, ny⟩

      match nd with
      | some nd => dist := update dist i0 i1 nd i1 1
      | none => ()

      for i2 in [0:5] do
        dist := update dist i0 i1 i0 i2 (base[i1]![i2]!)

  -- floyd warshall
  for i0 in [0:11] do
    for i1 in [0:5] do
      for i2 in [0:11] do
        for i3 in [0:5] do
          for i4 in [0:11] do
            for i5 in [0:5] do
              dist := update dist i2 i3 i4 i5 (dist[i2]![i3]![i0]![i1]! + dist[i0]![i1]![i4]![i5]!)

  -- Compute main key pad
  let mut result := baseKeyPad 11
  for i in [0:11] do
    for j in [0:11] do
      result := result.modify i (λ row => row.set! j dist[i]![1]![j]![1]!)
  return result

def charToDigit (c : Char) : ℕ :=
  match c with
  | '0' => 0
  | '1' => 1
  | '2' => 2
  | '3' => 3
  | '4' => 4
  | '5' => 5
  | '6' => 6
  | '7' => 7
  | '8' => 8
  | '9' => 9
  | 'A' => 10
  | _ => panic! "impossible"

def solve_dist (code : String) (base : Array (Array ℕ)) : ℕ := Id.run do
  let mut ans := 0
  let mut pos := 10

  for c in code.toList do
    ans := ans + base[pos]![charToDigit c]! + 1
    pos := charToDigit c

  return ans

def solve_one (code : String) (base : Array (Array ℕ)) : ℕ :=
  (solve_dist code base) * (code.take 3 |>.toNat!)

def solve (codes : List String) (numKeyPads : ℕ): ℕ := Id.run do
  let mut base := baseKeyPad 5
  for _ in [0:numKeyPads] do
    base := nextKeyPad base
  base := mainKeyPad base
  codes.map (solve_one · base) |>.sum

def main : IO Unit := IO.interact $ λ input ↦
  let codes := lines input
  s!"Part1: {solve codes 2}\n" ++
  s!"Part2: {solve codes 25}\n"
