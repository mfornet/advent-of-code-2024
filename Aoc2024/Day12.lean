import Aoc2024.Lib.Utils
import Std.Data.HashSet.Basic
import Batteries.Data.HashMap.Basic

abbrev Map (α : Type) := Array (Array α)

def parseInput (input : String) : Map Char :=
  lines input |>.map (·.toList.toArray) |>.toArray

structure Region where
  area : ℕ
  perimeter : ℕ
deriving BEq

instance : ToString Region where
  toString r := s!"Region {r.area} {r.perimeter}"

def Region.price (r : Region) : ℕ := r.area * r.perimeter

def get? {α : Type} (map : Map α) (x y : ℤ) : Option α := do
  if x < 0 || y < 0 || x >= map.size || (y >= (map.get! x.toNat |>.size)) then
    none
  else
    let row ← map.get? x.toNat
    let cell ← row.get? y.toNat
    some cell

def get! {α : Type} [Inhabited α] (map : Map α) (x y : ℤ) : α :=
  match get? map x y with
  | some x => x
  | none => panic! "get!: out of bounds"

partial def dfsRec (bulkDiscount : Bool) (map : Map Char) (visited : Map Bool) (x y : ℤ) (region: Region) : IO $ (Map Bool) × Region := do
  match get? visited x y with
  | some false =>
    let value := get! map x y
    let region := { region with area := region.area + 1 }
    let visited := visited.modify x.toNat (·.modify y.toNat (λ _ => true))

    [(1, 0), (0,  1), (-1, 0), (0, -1)].foldlM (λ (visited, region) (dx, dy) => do
      let nx := x + dx
      let ny := y + dy

      let region := if bulkDiscount then
        let dx' := -dy
        let dy' := dx
        let nx' := x + dx'
        let ny' := y + dy'
        let a := get? map nx ny != some value
        let b := get? map nx' ny' != some value
        let outerCorner := a && b

        let a := get? map nx ny == some value
        let b := get? map nx' ny' == some value
        let c := get? map (x + dx + dx') (y + dy + dy') != some value
        let innerCorner := a && b && c

        if outerCorner || innerCorner then
          { region with perimeter := region.perimeter + 1 }
        else
          region
      else region

      if get? map nx ny == some value then
        dfsRec bulkDiscount map visited nx ny region
      else
        let region := if bulkDiscount then
          region
        else
          { region with perimeter := region.perimeter + 1 }

        return (visited, region)
    ) (visited, region)
  | _ => return (visited, region)

def dfs (bulkDiscount : Bool) (map : Map Char) (visited : Map Bool) (x y : ℕ) : IO $ (Map Bool) × Region :=
  dfsRec bulkDiscount map visited (Int.ofNat x) (Int.ofNat y) { area := 0, perimeter := 0 }

def solve (map : Map Char) (bulkDiscount : Bool) : IO ℕ := do
  let mut visited := Array.mkArray map.size (Array.mkArray (map.get! 0 |>.size) false)
  let mut answer := 0

  for i in [0:map.size] do
    for j in [0:map.get! i |>.size] do
      if visited.get! i |>.get! j then continue
      let result ← dfs bulkDiscount map visited i j
      visited := result.1
      answer := answer + result.2.price

  return answer

def main : IO Unit := IO.interactM $ λ input ↦
  let map := parseInput input
  return s!"Part1: {← solve map false}\n" ++
  s!"Part2: {← solve map true}\n"
