import Aoc2024.Lib.Utils
import Std.Data.HashSet.Basic
import Batteries.Data.HashMap.Basic

structure Input where
  map : Array (Array Bool)
  start: ℤ × ℤ
  goal: ℤ × ℤ

def parseInput (input : String) : Input := Id.run do
  let l := lines input
  let n := l.length
  let m := l[0]!.length

  let mut start : (ℤ × ℤ) := ⟨0, 0⟩
  let mut goal : (ℤ × ℤ) := ⟨0, 0⟩
  let mut map := Array.mkArray n (Array.mkArray m false)

  for (i, line) in l.enum do
    for (j, c) in line.toList.enum do
      match c with
      | '#' => map := map.set! i (map[i]!.set! j true)
      | 'S' => start := ⟨i, j⟩
      | 'E' => goal := ⟨i, j⟩
      | _ => ()

  Input.mk map start goal

def directions : Array (ℤ × ℤ) := #[⟨-1, 0⟩, ⟨1, 0⟩, ⟨0, -1⟩, ⟨0, 1⟩]

def bfs (map : Array (Array Bool)) (start : ℤ × ℤ) : Array (Array (Option ℕ)) := Id.run do
  let n := map.size
  let m := map[0]!.size
  let mut dist := Array.mkArray n (Array.mkArray m (none : Option ℕ))
  let mut q := Std.Queue.empty.enqueue (⟨0, start⟩ : (ℕ × ℤ × ℤ))
  dist := dist.modify start.1.toNat (λ a => a.set! start.2.toNat (some 0))

  while true do
    match q.dequeue? with
    | none => break
    | some ((d, x, y), nq) =>
      q := nq

      for (dx, dy) in directions do
        let nx := x + dx
        let ny := y + dy

        if map[nx.toNat]![ny.toNat]! then
          continue

        if dist[nx.toNat]![ny.toNat]!.isNone then
          dist := dist.modify nx.toNat (λ a => a.set! ny.toNat (some (d + 1)))
          q := q.enqueue (d + 1, nx, ny)

  dist

def solve (input : Input) (save : ℕ): ℕ := Id.run do
  let dist_s := bfs input.map input.start
  let dist_g := bfs input.map input.goal

  let best := dist_s[input.goal.1.toNat]![input.goal.2.toNat]!.get!

  let n := input.map.size
  let m := input.map[0]!.size

  let mut ans := 0

  for x in [0:n] do
    for y in [0:m] do
      if input.map[x]![y]! then
        let x := Int.ofNat x
        let y := Int.ofNat y

        for (dx0, dy0) in directions do
          for (dx1, dy1) in directions do
            let nx0 := x + dx0
            let ny0 := y + dy0
            let nx1 := x + dx1
            let ny1 := y + dy1

            if 0 <= nx0 && nx0 < n && 0 <= ny0 && ny0 < m && 0 <= nx1 && nx1 < n && 0 <= ny1 && ny1 < m then
              if input.map[nx0.toNat]![ny0.toNat]! || input.map[nx1.toNat]![ny1.toNat]! then
                continue

              let d0 := dist_s[nx0.toNat]![ny0.toNat]!.get!
              let d1 := dist_g[nx1.toNat]![ny1.toNat]!.get!

              let new_dist := d0 + d1 + 2

              -- dbg_trace (x, y, nx0, ny0, nx1, ny1, d0, d1, new_dist)

              if new_dist + save <= best then
                ans := ans + 1

  return ans

def solve2 (input : Input) (cheat : ℕ) (save : ℕ) : ℕ := Id.run do
  let dist_s := bfs input.map input.start
  let dist_g := bfs input.map input.goal

  let best := dist_s[input.goal.1.toNat]![input.goal.2.toNat]!.get!

  let n := input.map.size
  let m := input.map[0]!.size

  let mut ans := 0

  for x0 in [0:n] do
    for y0 in [0:m] do
      if !input.map[x0]![y0]! then
        for x1 in [0:n] do
          for y1 in [0:m] do
            if !input.map[x1]![y1]! then
              if (absDiff x0 x1) + (absDiff y0 y1) <= cheat then
                let d0 := dist_s[x0]![y0]!.get!
                let d1 := dist_g[x1]![y1]!.get!
                let new_dist := d0 + d1 + (absDiff x0 x1) + (absDiff y0 y1)
                if new_dist + save <= best then
                  -- dbg_trace (x0, y0, x1, y1, d0, d1, new_dist)
                  ans := ans + 1

  return ans

def main : IO Unit := IO.interact $ λ input ↦
  let input := parseInput input
  s!"Part1: {solve2 input 2 100}\n" ++
  s!"Part2: {solve2 input 20 100}\n"
