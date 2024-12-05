import Aoc2024.Lib.Utils
import Std.Data.HashSet.Basic
import Batteries.Data.HashMap.Basic

abbrev Rule := ℕ × ℕ
abbrev Update := List ℕ

structure Input where
  rules : List Rule
  updates : List Update
deriving Repr

def parseNat (s : String) : ℕ :=
  let n := s.toNat?
  match n with
  | some n => n
  | none => panic! s!"parseNat: {s}"

def print (msg : String) : IO Unit :=
  IO.println msg

def parseInput (input : String) : Input :=
  let ⟨rules, updates⟩ := input.splitOn "\n\n" |>.first2!
  let rules := lines rules |>.map $ λ line ↦ line.splitOn "|" |>.map parseNat |>.first2!
  let updates := lines updates |>.map $ λ line ↦ line.splitOn "," |>.map parseNat
  Input.mk rules updates

def Input.fastRules (self : Input) : Std.HashSet Rule :=
  self.rules.foldl (λ s r ↦ s.insert r) Std.HashSet.empty

def checkOrdered (rules : Std.HashSet Rule) (update : Update) : Bool :=
  match update with
  | [] => true
  | x :: xs => xs.all (λ y ↦ !rules.contains (y, x)) && checkOrdered rules xs

def Update.midElement (self : Update) : ℕ :=
  let n := self.length
  self.get! (n / 2)

def Input.countOrdered (self : Input) : ℕ :=
  let rules := self.fastRules
  self.updates.filter (checkOrdered rules) |>.map Update.midElement |>.foldl .add 0

abbrev Indegree := Batteries.HashMap ℕ ℕ
abbrev Queue := Std.Queue ℕ

structure Graph where
  adjacency : Batteries.HashMap ℕ (List ℕ)
  indegree : Indegree

def Graph.addEdge (self : Graph) (src dst : ℕ) : Graph :=
  let adjacency := self.adjacency.insert src (dst :: self.adjacency.findD src [])
  let indegree := self.indegree.insert dst (self.indegree.findD dst 0 + 1)
  Graph.mk adjacency indegree

def Graph.empty : Graph :=
  Graph.mk Batteries.HashMap.empty Batteries.HashMap.empty

def decreaseIndegree (indegree : Indegree) (queue : Queue) (dst : ℕ) : Indegree × Queue :=
  let prev := indegree.find! dst
  let indegree := indegree.insert dst (prev - 1)
  if prev == 1 then
    (indegree, queue.enqueue dst)
  else
    (indegree, queue)

-- Remove partial and prove termination using (sum of the indegrees, |queue|)
partial def topoRec (queue : Queue) (graph : Graph) (order : Array ℕ) : Array ℕ :=
  match queue.dequeue? with
  | none => order
  | some (node, queue) =>
    let order := order.push node
    let neighbors := graph.adjacency.findD node []
    let indegree := graph.indegree
    let ⟨indegree, queue⟩ := neighbors.foldl (λ state neighbor ↦ decreaseIndegree state.1 state.2 neighbor) ⟨indegree, queue⟩
    topoRec queue (Graph.mk graph.adjacency indegree) order

def List.unique (l : List ℕ) : List ℕ :=
  l.foldl (λ seen n ↦ seen.insert n) Std.HashSet.empty |>.toList

def findTopologicalOrder (rules : List Rule) : Debug $ Array ℕ := do
  let graph := rules.foldl (λ g r ↦ g.addEdge r.1 r.2) Graph.empty

  -- Initial nodes of degree 0
  let initial := rules.map Prod.fst |>.filter (not ∘ graph.indegree.contains) |>.unique

  -- Queue of nodes with all incoming edges removed
  let queue := Std.Queue.empty.enqueueAll initial

  ok $ topoRec queue graph Array.empty

def reverseArray (a : Array ℕ) : Batteries.HashMap ℕ ℕ :=
  a.foldl (λ m n ↦ m.insert n m.size) Batteries.HashMap.empty

def topologicalSort (a : Array ℕ) (edges : List Rule) : Debug $ Array ℕ := do
  let revA := reverseArray a
  let rules := edges.filter (λ e ↦ revA.contains e.1 && revA.contains e.2)
  let topo ← findTopologicalOrder rules
  let revTopo := reverseArray topo
  let order := a.qsort (λ x y ↦ revTopo.find! x < revTopo.find! y)
  ok order

def midElement (a : Array ℕ) : ℕ :=
  let n := a.size
  a.get! (n / 2)

def Input.countUnordered (self : Input) : Debug ℕ := do
  let rules := self.fastRules
  let ap ← self.updates.filter (not ∘ checkOrdered rules)
           |>.mapM (λ update ↦ (topologicalSort update.toArray self.rules))
  ok $ ap.map midElement |>.foldl .add 0

def main : IO Unit := IO.interact $ λ input ↦
  let input := parseInput input
  s!"Part1: {input.countOrdered}\n" ++
  s!"Part2: {input.countUnordered.value}\n"
