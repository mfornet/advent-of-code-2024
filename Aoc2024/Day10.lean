import Aoc2024.Lib.Utils
import Std.Data.HashSet.Basic
import Batteries.Data.HashMap.Basic

abbrev Map := Array $ Array ℕ

def Map.shape (m : Map) : (ℕ × ℕ) :=
  (m.size, m.get! 0 |>.size)

def parseInput (input : String) : Map :=
  lines input |>.map (λ line ↦ line.toList.map DigitToNat! |>.toArray) |>.toArray

structure TrailRoutePositions where
  height : ℕ
  positions : Batteries.HashMap (ℤ × ℤ) ℕ

def Map.startTrailRoutePositions (map : Map) : TrailRoutePositions :=
  let positions := map.toList.enum.flatMap (λ (x, row) => row.toList.enum.filter (·.snd == 0) |>.map (λ (y, _) => (Int.ofNat x, Int.ofNat y)))
    |>.foldl (λ m p ↦ m.insert p 1) ∅
  { height := 0, positions := positions }

def Map.step (map : Map) (positions : TrailRoutePositions) : TrailRoutePositions :=
  let new_height := positions.height + 1
  let new_positions := positions.positions.toList.flatMap (λ ((x, y), v) => [(x + 1, y, v), (x, y + 1, v), (x - 1, y, v), (x, y - 1 ,v)])
    |>.filter (λ (x, y, _) => 0 <= x && x < map.shape.fst && 0 <= y && y < map.shape.snd)
    |>.filter (λ (x, y, _) => (map.get! x.toNat |>.get! y.toNat) == new_height)
    |>.foldl (λ s (x, y, v) => match s.find? (x, y) with
      | none => s.insert (x, y) v
      | some v' => s.insert (x, y) (v + v')
    ) ∅
  { height := new_height, positions := new_positions }

def Map.multipleStep (map : Map) (positions : TrailRoutePositions) (n : ℕ) : TrailRoutePositions :=
  match n with
  | 0 => positions
  | n' + 1 => map.multipleStep (map.step positions) n'

def solve (map : Map) : ℕ :=
  map.startTrailRoutePositions.positions.toList.map (λ p => { height := 0, positions := Batteries.HashMap.ofList [p] })
    |>.map (λ p => map.multipleStep p 9 |>.positions.size)
    |>.sum

def solve2 (map : Map) : ℕ :=
  map.startTrailRoutePositions.positions.toList.map (λ p => { height := 0, positions := Batteries.HashMap.ofList [p] })
    |>.flatMap (λ p => map.multipleStep p 9 |>.positions.toList)
    |>.map (·.snd)
    |>.sum

def main : IO Unit := IO.interact $ λ input ↦
  let map := parseInput input
  s!"Part1: {solve map}\n" ++
  s!"Part2: {solve2 map}\n"
