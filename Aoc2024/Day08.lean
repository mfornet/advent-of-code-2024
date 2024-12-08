import Aoc2024.Lib.Utils
import Std.Data.HashSet.Basic
import Batteries.Data.HashMap.Basic

structure Map where
  shape : ℕ × ℕ
  antennas : Std.HashMap Char (List (ℤ × ℤ))

def parseInput (input : String) : Map :=
  lines input |>.enum |>.foldl (λ map ⟨row, line⟩ ↦
  line.toList |>.enum |>.foldl (λ map ⟨col, c⟩ ↦
    match c with
    | '.' => { map with shape := ⟨row + 1, col + 1⟩ }
    | c => (
      match map.antennas.get? c with
      | none => { shape := ⟨row + 1, col + 1⟩, antennas := map.antennas.insert c [⟨Int.ofNat row, Int.ofNat col⟩] }
      | some list => { shape := ⟨row + 1, col + 1⟩, antennas := map.antennas.insert c (list.insert (row, col)) }
    )
  ) map
  ) { shape := ⟨0, 0⟩, antennas := Std.HashMap.empty }

def findAntinodes' (positions : List (ℤ × ℤ)) (shape : ℕ × ℕ) : Std.HashSet (ℤ × ℤ) :=
  let ⟨n, m⟩ := (Int.ofNat shape.1, Int.ofNat shape.2)
  positions.flatMap (λ p₀ => positions.map (λ p₁ => (p₀, p₁)))
    |>.filter (λ ⟨p₀, p₁⟩ => p₀ != p₁)
    |>.map (λ ⟨p₀, p₁⟩ => (2 * p₀.1 - p₁.1, 2 * p₀.2 - p₁.2))
    |>.filter (λ ⟨x, y⟩ => 0 <= x && x < n && 0 <= y && y < m)
    |>.foldl .insert ∅

partial def extendRec (p d : ℤ × ℤ) (bound : ℤ × ℤ) : List (ℤ × ℤ) :=
  if 0 <= p.1 && p.1 < bound.1 && 0 <= p.2 && p.2 < bound.2 then
    p :: extendRec (p.1 + d.1, p.2 + d.2) d bound
  else
    []

def findAlignedAntinodes' (positions : List (ℤ × ℤ)) (shape : ℕ × ℕ) : Std.HashSet (ℤ × ℤ) :=
  let ⟨n, m⟩ := (Int.ofNat shape.1, Int.ofNat shape.2)
  positions.flatMap (λ p₀ => positions.map (λ p₁ => (p₀, p₁)))
    |>.filter (λ ⟨p₀, p₁⟩ => p₀ != p₁)
    |>.flatMap (λ ⟨p₀, p₁⟩ =>
      let ⟨dx, dy⟩ := (p₁.1 - p₀.1, p₁.2 - p₀.2)
      let g := Nat.gcd dx.natAbs dy.natAbs
      let ⟨dx, dy⟩ := (dx / g, dy / g)
      extendRec p₀ (dx, dy) (n, m) ++
      extendRec p₀ (-dx, -dy) (n, m)
    )
    |>.foldl .insert ∅

def Map.findAntinodes (map : Map) (antinodes : List (ℤ × ℤ) → (ℕ × ℕ) → Std.HashSet (ℤ × ℤ)) : Std.HashSet (ℤ × ℤ) :=
  map.antennas.toList.map (λ ⟨_, positions⟩ ↦ antinodes positions map.shape)
  |>.foldl Std.HashSet.union Std.HashSet.empty

def Map.solve (map : Map) (antinodes : List (ℤ × ℤ) → (ℕ × ℕ) → Std.HashSet (ℤ × ℤ)) : ℕ :=
  map.findAntinodes antinodes |>.size

def main : IO Unit := IO.interact $ λ input ↦
  let map := parseInput input
  s!"Part1: {map.solve findAntinodes'}\n" ++
  s!"Part2: {map.solve findAlignedAntinodes'}\n"
