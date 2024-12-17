import Aoc2024.Lib.Utils
import Std.Data.HashSet.Basic
import Batteries.Data.HashMap.Basic

structure Board where
  boxes: Std.HashSet (ℤ × ℤ)
  walls: Std.HashSet (ℤ × ℤ)
  position: ℤ × ℤ

instance : ToString Board where
  toString board := s!"boxes: {board.boxes.toList}\nposition: {board.position}"

inductive Step where
  | up
  | down
  | left
  | right
deriving Inhabited

instance : ToString Step where
  toString
    | Step.up => "up"
    | Step.down => "down"
    | Step.left => "left"
    | Step.right => "right"

def parseBoard (input : String) : Board :=
  lines input |>.enum |>.foldl (λ board (row, line) =>
    line.toList |>.enum |>.foldl (λ board (col, c) =>
      match c with
      | '#' => { board with walls := board.walls.insert ⟨row, col⟩ }
      | 'O' => { board with boxes := board.boxes.insert ⟨row, col⟩ }
      | '@' => { board with position := ⟨row, col⟩ }
      | _ => board
    ) board
  ) (Board.mk ∅ ∅ ⟨0, 0⟩)

def parseInput (input : String) : Board × (List Step) :=
  let (board, moves) := input.splitOn "\n\n" |>.first2!
  let moves := moves.replace "\n" "" |>.toList |>.map (λ c => match c with
    | '^' => Step.up
    | 'v' => Step.down
    | '<' => Step.left
    | '>' => Step.right
    | _ => panic! s!"parseInput: {c}"
  )
  ⟨parseBoard board, moves⟩

def Step.direction (step : Step) : ℤ × ℤ :=
  match step with
  | Step.up => ⟨-1, 0⟩
  | Step.down => ⟨1, 0⟩
  | Step.left => ⟨0, -1⟩
  | Step.right => ⟨0, 1⟩

def Board.step (board : Board) (step : Step) : Board := Id.run do
  let (dx, dy) := step.direction
  let (px, py) := board.position
  let mut x := px + dx
  let mut y := py + dy
  let mut board := board

  while true do
    if board.walls.contains ⟨x, y⟩ then
      break

    if !board.boxes.contains ⟨x, y⟩ then
      let nx := px + dx
      let ny := py + dy

      board := { board with position := ⟨nx, ny⟩ }
      board := { board with boxes := board.boxes.insert ⟨x, y⟩ }
      board := { board with boxes := board.boxes.erase ⟨nx, ny⟩ }

      break

    x := x + dx
    y := y + dy

  return board

def Board.eval (board : Board) : ℕ :=
  board.boxes.toList.map (λ (x, y) => 100 * Int.toNat x + Int.toNat y) |>.sum

def Board.isBox (board : Board) (x y : ℤ) : Bool :=
  board.boxes.contains ⟨x, y⟩ || board.boxes.contains ⟨x, y - 1⟩

def Board.getBoxOther (board : Board) (x y : ℤ) : ℤ × ℤ :=
  if board.boxes.contains ⟨x, y⟩ then
    (x, y + 1)
  else
    (x, y - 1)

def Board.shape (board : Board) : ℕ × ℕ :=
  let (x, y) := board.walls.toList |>.foldl (λ (max_x, max_y) (x, y) => (x.toNat.max max_x, y.toNat.max max_y)) (0, 0)
  ⟨x + 1, y + 1⟩

def updateMap (map : Array (Array Char)) (p : ℤ × ℤ) (v : Char) : Array (Array Char) :=
  let x := Int.toNat p.1
  let y := Int.toNat p.2
  map.modify x (λ row => row.modify y (λ _ => v))

def Board.toMap2 (board : Board) : Array (Array Char) :=
  let (n, m) := board.shape
  let map := Array.mkArray n (Array.mkArray m '.')
  let map := updateMap map board.position '@'
  let map := board.walls.toList.foldl (λ map (x, y) => updateMap map (x, y) '#') map
  let map := board.boxes.toList.foldl (λ map (x, y) => updateMap map (x, y) '[') map
  let map := board.boxes.toList.foldl (λ map (x, y) => updateMap map (x, y + 1) ']') map
  map

def Board.toPrettyString (board : Board) : String :=
  String.join $ board.toMap2.toList.map (λ row => String.join $ row.toList.map (λ c => c.toString)) |>.map (λ row => row ++ "\n")

def Board.step2 (board : Board) (step : Step) : Board := Id.run do
  let (dx, dy) := step.direction
  let mut active := #[board.position]
  let mut seen : Std.HashSet (ℤ × ℤ) := { board.position }
  let mut ok := true
  let mut board := board

  while ok && !active.isEmpty do
    let (x, y) := active.back!
    -- dbg_trace s!"x={x} y={y}"
    active := active.pop

    let (nx, ny) := (x + dx, y + dy)

    if board.walls.contains ⟨nx, ny⟩ then
      ok := false
      break

    if board.isBox nx ny then
      if !seen.contains (nx, ny) then
        seen := seen.insert (nx, ny)
        active := active.push (nx, ny)

        let (ox, oy) := board.getBoxOther nx ny

        seen := seen.insert (ox, oy)
        active := active.push (ox, oy)

  -- dbg_trace s!"ok={ok}"
  if ok then
    let (px, py) := board.position
    let changed := board.boxes.toList.filter (λ (x, y) => seen.contains (x, y))
    let boxes := changed.foldl (λ set p => set.erase p) board.boxes
    let boxes := changed.foldl (λ set (x, y) => set.insert (x + dx, y + dy)) boxes
    board := { board with position := ⟨px + dx, py + dy⟩, boxes := boxes }

  -- dbg_trace s!"{step}"
  -- dbg_trace s!"{board.toPrettyString}\n\n"
  return board

def solve (board : Board) (steps : List Step) (step_f : Board → Step → Board) : ℕ :=
  steps.foldl step_f board |>.eval

def Board.toBoard2 (board : Board) : Board :=
  {
    position := ⟨board.position.1, board.position.2 * 2⟩
    boxes := board.boxes.toList.map (λ (x, y) => ⟨x, y * 2⟩) |>.toHashSet
    walls := board.walls.toList.flatMap (λ (x, y) => [⟨x, y * 2⟩, ⟨x, y * 2 + 1⟩]) |>.toHashSet
  }

def main : IO Unit := IO.interact $ λ input ↦
  let (board, steps) := parseInput input
  s!"Part1: {solve board steps Board.step}\n" ++
  s!"Part2: {solve board.toBoard2 steps Board.step2}\n"
