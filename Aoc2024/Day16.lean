import Aoc2024.Lib.Utils
import Std.Data.HashSet.Basic
import Batteries.Data.HashMap.Basic
import Batteries.Data.BinaryHeap

abbrev Map := Array (Array Bool)

structure Input where
  map : Map
  start : (ℤ × ℤ)
  goal : (ℤ × ℤ)

def parseInput (input : String) : Input := Id.run do
  let lines := lines input |>.map (λ l => l.toList.toArray) |>.toArray
  let n := lines.size
  let m := lines.get! 0 |>.size
  let mut map := Array.mkArray n (Array.mkArray m false)
  let mut start := (0, 0)
  let mut goal := (0, 0)

  for i in [0:n] do
    for j in [0:m] do
      if (lines.get! i |>.get! j) == '#' then
        map := map.modify i λ row ↦ row.modify j λ _ ↦ true

      if (lines.get! i |>.get! j) == 'S' then
        start := (Int.ofNat i, Int.ofNat j)

      if (lines.get! i |>.get! j) == 'E' then
        goal := (Int.ofNat i, Int.ofNat j)

  Input.mk map start goal

inductive Direction where
  | Up
  | Down
  | Left
  | Right
deriving DecidableEq, Hashable, Inhabited

instance : ToString Direction where
  toString d := match d with
  | Direction.Up => "Up"
  | Direction.Down => "Down"
  | Direction.Left => "Left"
  | Direction.Right => "Right"

def directions : List Direction := [Direction.Up, Direction.Down, Direction.Left, Direction.Right]

def Direction.direction (direction : Direction) : (ℤ × ℤ) :=
  match direction with
  | Direction.Up => (-1, 0)
  | Direction.Down => (1, 0)
  | Direction.Left => (0, -1)
  | Direction.Right => (0, 1)

def Direction.orthogonal (direction : Direction) : Direction × Direction :=
  match direction with
  | Direction.Up => ⟨Direction.Left, Direction.Right⟩
  | Direction.Down => ⟨Direction.Left, Direction.Right⟩
  | Direction.Left => ⟨Direction.Up, Direction.Down⟩
  | Direction.Right => ⟨Direction.Up, Direction.Down⟩

def Direction.opposite (direction : Direction) : Direction :=
  match direction with
  | Direction.Up => Direction.Down
  | Direction.Down => Direction.Up
  | Direction.Left => Direction.Right
  | Direction.Right => Direction.Left

def dijkstra (map: Map) (start: (ℤ × ℤ × Direction)) : Std.HashMap (ℤ × ℤ × Direction) ℕ := Id.run do
  let mut queue := Batteries.BinaryHeap.empty ( α := ℕ × ℤ × ℤ × Direction) (λ (a b) => a.1 > b.1)
  let mut distance := Std.HashMap.empty (α := ℤ × ℤ × Direction) (β := ℕ)

  queue := queue.insert (0, start)
  distance := distance.insert start 0

  while (queue.size) > 0 do
    let (cost, x, y, direction) := queue.max.get!
    queue := queue.popMax

    if distance.get! (x, y, direction) < cost then
      continue

    let (dx, dy) := direction.direction
    let nx := x + dx
    let ny := y + dy

    let ⟨dl, dr⟩ := direction.orthogonal

    let next := [(nx, ny, direction, 1), (x, y, dl, 1000), (x, y, dr, 1000)]

    for (nx, ny, nd, nc) in next do
      if map.get! (Int.toNat nx) |>.get! (Int.toNat ny) then
        continue

      let better := distance.get? (nx, ny, nd) |>.map (λ c => (cost + nc < c : Bool)) |>.getD true

      if better then
        distance := distance.insert (nx, ny, nd) (cost + nc)
        queue := queue.insert (cost + nc, nx, ny, nd)

  return distance

def solve (input : Input) : ℕ :=
  let distance := dijkstra input.map ⟨input.start.1, input.start.2, Direction.Right⟩
  directions
  |>.filterMap (λ d => distance.get? ⟨input.goal.1, input.goal.2, d⟩)
  |>.foldl (λ a b => a.min b) 1000000

def solve2 (input : Input) : ℕ := Id.run do
  let distance0 := dijkstra input.map ⟨input.start.1, input.start.2, Direction.Right⟩
  let mut best_direction := Direction.Right

  for d in directions do
    if distance0.get! ⟨input.goal.1, input.goal.2, d⟩ < distance0.get! ⟨input.goal.1, input.goal.2, best_direction⟩ then
      best_direction := d

  let best := distance0.get! ⟨input.goal.1, input.goal.2, best_direction⟩
  let distance1 := dijkstra input.map ⟨input.goal.1, input.goal.2, best_direction.opposite⟩

  let mut tiles : Std.HashSet (ℤ × ℤ) := ∅

  for ((x, y, d), c0) in distance0 do
    let c1 := distance1.get! (x, y, d.opposite)
    -- dbg_trace s!"{x}\t{y}\t{d}\t{c0}\t{c1}\t{best}"
    if c0 + c1 == best then
      tiles := tiles.insert (x, y)

  tiles.size

def main : IO Unit := IO.interact $ λ input ↦
  let input := parseInput input
  s!"Part1: {solve input}\n" ++
  s!"Part2: {solve2 input}\n"
