import Aoc2024.Lib.Utils
import Std.Data.HashSet.Basic
import Batteries.Data.HashMap.Basic

structure Graph where
  names : Array String
  adj : Array (Array Bool)

structure FindClique where
  adj : Array (Array ℕ)
  epoch : ℕ
  active : Array ℕ
  degree : Array ℕ

def Graph.toFindClique (graph : Graph) : FindClique := Id.run do
  let mut adj := Array.mkArray graph.adj.size (Array.mkArray 0 0)
  let mut degree := Array.mkArray graph.adj.size 0
  let active := Array.mkArray graph.adj.size 0

  let n := graph.adj.size
  for u in [0:n] do
    for v in [0:n] do
      if graph.adj[u]![v]! then
        adj := adj.modify u (λ a => a.push v)
        degree := degree.modify u (λ d => d + 1)

  FindClique.mk adj 0 active degree


def parseInput (input : String) : Graph := Id.run do
  let mut names : Array String := #[]
  let mut rev : Batteries.HashMap String ℕ := ∅

  for line in lines input do
    for name in line.splitOn "-" do
      if !rev.contains name then
        rev := rev.insert name names.size
        names := names.push name

  let mut adj := Array.mkArray names.size (Array.mkArray names.size false)

  for line in lines input do
    let (u, v) := line.splitOn "-" |>.map (λ name ↦ rev.find! name) |>.first2!
    adj := adj.modify u (λ a => a.modify v (λ _ => true))
    adj := adj.modify v (λ a => a.modify u (λ _ => true))

  Graph.mk names adj

def Graph.firstChar (graph : Graph) (u : ℕ) : Char :=
  graph.names[u]!.toList.head!

def solve (graph : Graph) : ℕ := Id.run do
  let mut (answer : ℕ) := 0

  let n := graph.names.size
  for u in [0:n] do
    for v in [0:u] do
      for w in [0:v] do
        if graph.firstChar u != 't' &&
           graph.firstChar v != 't' &&
           graph.firstChar w != 't' then
          continue

        if graph.adj[u]![v]! && graph.adj[v]![w]! && graph.adj[w]![u]! then
          answer := answer + 1

  answer

partial def findClique (graph : FindClique) (clique : Array ℕ) : Array ℕ := Id.run do
  let n := graph.adj.size
  let mut best := (none : Option ℕ)

  for u in [0:n] do
    if graph.active[u]! != graph.epoch then
      continue

    match best with
    | none => best := some u
    | some v =>
      if graph.degree[u]! > graph.degree[v]! then
        best := some u

  match best with
  | none => return clique
  | some u =>
    -- Check if `u` is part of the clique
    let mut graph0 := graph
    let clique0 := clique.push u

    let epoch := graph.epoch
    let new_epoch := epoch + 1

    for v in graph0.adj[u]! do
      if graph0.active[v]! == graph0.epoch then
        graph0 := { graph0 with
          degree := graph0.degree.modify v (λ d => d - 1),
          active := graph0.active.modify v (λ _ => new_epoch)
        }

    graph0 := { graph0 with epoch := new_epoch }
    let clique0 := findClique graph0 clique0

    -- Check if `u` is not part of the clique
    let mut graph1 := graph
    for v in graph1.adj[u]! do
      if graph1.active[v]! == graph1.epoch then
        graph1 := { graph1 with
          degree := graph1.degree.modify v (λ d => d - 1),
        }

    graph1 := { graph1 with active := graph1.active.set! u 1000000000 }
    let clique1 := findClique graph1 clique

    if clique0.size > clique1.size then
      clique0
    else
      clique1

def solve2 (graph : Graph) : String :=
  let helper_graph := graph.toFindClique
  let clique := findClique helper_graph #[] |>.map (λ u ↦ graph.names[u]!)
  let clique := clique.qsort (λ a b ↦ a < b)
  String.intercalate "," clique.toList

def main : IO Unit := IO.interact $ λ input ↦
  let graph := parseInput input
  s!"Part1: {solve graph}\n" ++
  s!"Part2: {solve2 graph}\n"
