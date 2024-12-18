import Aoc2024.Lib.Utils
import Std.Data.HashSet.Basic
import Batteries.Data.HashMap.Basic

def parseInput (input : String) : List (ℤ × ℤ) :=
  lines input |>.map (λ l => l.splitOn "," |>.map (λ d => Int.ofNat d.toNat!) |>.first2!)

def buildMap (bytes : List (ℤ × ℤ)) : Array (Array Bool) :=
  let (n, m) := bytes.foldl (λ top p => ⟨top.1.max p.1, top.2.max p.2⟩) (⟨0, 0⟩ : ℤ × ℤ)
  let map := Array.mkArray (n.toNat + 1) (Array.mkArray (m.toNat + 1) false)
  bytes.foldl (λ map (x, y) => map.modify y.toNat (λ row => row.modify x.toNat (λ _ => true))) map

def directions : List (ℤ × ℤ) :=
  [(1, 0), (-1, 0), (0, 1), (0, -1)]

def prettyPrint (map : Array (Array Bool)) : String :=
  String.intercalate "\n" (map.toList.map (λ row => String.intercalate "" (row.toList.map (λ b => if b then "#" else "."))))

def findDistance (map : Array (Array Bool)) : Option ℕ := Id.run do
  -- dbg_trace s!"{prettyPrint map}"
  let mut distance := map.map (λ row => row.map (λ _ => (none : Option ℕ)))
  distance := distance.modify 0 (λ row => row.modify 0 (λ _ => some 0))
  let mut queue : Std.Queue (ℤ × ℤ) := ∅
  queue := queue.enqueue (0, 0)

  let n := map.size
  let m := map[0]!.size

  while true do
    match queue.dequeue? with
    | none => break
    | some ((x, y), nqueue) =>
      queue := nqueue
      let d := distance.get! x.toNat |>.get! y.toNat |>.get!
      -- dbg_trace s!"{x} {y} {d}"

      for (dx, dy) in directions do
        let nx := x + dx
        let ny := y + dy

        if nx < 0 || nx >= n || ny < 0 || ny >= m || map[nx.toNat]![ny.toNat]! then
          continue

        match distance[nx.toNat]![ny.toNat]! with
        | none =>
          distance := distance.modify nx.toNat (λ row => row.modify ny.toNat (λ _ => some (d + 1)))
          queue := queue.enqueue (nx, ny)
        | _ => continue


  distance.back!.back!

def solve (map : Array (Array Bool)) : ℕ :=
  findDistance map |>.get!

def solve2 (bytes : List (ℤ × ℤ)) : ℤ × ℤ := Id.run do
  let (n, m) := bytes.foldl (λ top p => ⟨top.1.max p.1, top.2.max p.2⟩) (⟨0, 0⟩ : ℤ × ℤ)
  let mut map := Array.mkArray (n.toNat + 1) (Array.mkArray (m.toNat + 1) false)

  -- We can do binary search next time
  for (x, y) in bytes do
    map := map.modify y.toNat (λ row => row.modify x.toNat (λ _ => true))
    match findDistance map with
    | none => return ⟨x, y⟩
    | _ => continue

  ⟨0, 0⟩

def main : IO Unit := IO.interact $ λ input ↦
  let bytes := parseInput input
  s!"Part1: {solve (buildMap $ bytes.take 1024)}\n" ++
  s!"Part2: {solve2 bytes}\n"
