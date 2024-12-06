import Aoc2024.Lib.Utils
import Std.Data.HashSet.Basic
import Batteries.Data.HashMap.Basic

abbrev Obstacles := Std.HashSet (ℤ × ℤ)

structure Board where
  shape : ℤ × ℤ
  obstacles : Std.HashSet (ℤ × ℤ)
  position : ℤ × ℤ
  direction : ℤ × ℤ
deriving Repr

def rotate (d : ℤ × ℤ) : ℤ × ℤ :=
  (d.2, -d.1)

def Board.new : Board :=
  { shape := (0, 0), obstacles := ∅, position := (0, 0), direction := (-1, 0) }

def Board.parse (input : String) : Board :=
  lines input |>.enum |>.foldl (λ board (row, line) =>
    line.toList.enum.foldl (λ board (col, c) =>
      let board := { board with shape := (row + 1, col + 1) }
      match c with
      | '#' => { board with obstacles := board.obstacles.insert (row, col) }
      | '^' => { board with position := (row, col)}
      | _ => board
    ) board
  ) Board.new

def Board.inside (b : Board) (p : ℤ × ℤ) : Bool :=
  0 ≤ p.1 ∧ p.1 < b.shape.1 ∧ 0 ≤ p.2 ∧ p.2 < b.shape.2

def Board.emptySquare (b : Board) (p : ℤ × ℤ) : Bool :=
  ¬b.obstacles.contains p

inductive Path where
  | Escape
  | Loop
deriving Inhabited, BEq

structure Simulator where
  board : Board
  visited : Std.HashSet (ℤ × ℤ)
  visited_with_direction : Std.HashSet ((ℤ × ℤ) × (ℤ × ℤ))

def move (p d : ℤ × ℤ) : ℤ × ℤ :=
  (p.1 + d.1, p.2 + d.2)

def Simulator.step (s : Simulator) : Simulator × Option Path :=
  if s.visited_with_direction.contains (s.board.position, s.board.direction) then
    (s, some Path.Loop)
  else
    let s := { s with visited := s.visited.insert s.board.position, visited_with_direction := s.visited_with_direction.insert (s.board.position, s.board.direction) }

    let new_pos := move s.board.position s.board.direction
    match s.board.inside new_pos with
    | true => match s.board.emptySquare new_pos with
      | true => ({ s with board := { s.board with position := new_pos } }, none)
      | false => ({ s with board := { s.board with direction := rotate s.board.direction } }, none)
    | false => (s, some Path.Escape)

partial def Simulator.simulate (s : Simulator) : Simulator × Path :=
  match s.step with
  | (s', some p) => (s', p)
  | (s', none) => s'.simulate

def Board.simulate (b : Board) : Simulator × Path :=
  Simulator.mk b ∅ ∅ |>.simulate

def Board.numVisited (b : Board) : ℕ :=
  b.simulate |>.fst |>.visited |>.size

def gridPositions (n m : ℕ) : List (ℕ × ℕ) :=
  List.range n |>.flatMap (λ i ↦ List.range m |>.map (λ j ↦ (i, j)))

def Board.countStuckInLoopObstacles (b : Board) : ℕ :=
  gridPositions b.shape.1.toNat b.shape.2.toNat
  |>.map (λ p ↦ (Int.ofNat p.1, Int.ofNat p.2))
  |>.filter (not ∘ b.obstacles.contains)
  |>.filter (λ p => b.position != p)
  |>.filter (λ p =>
    let nb := { b with obstacles := b.obstacles.insert p }
    nb.simulate |>.snd == Path.Loop
  )
  |>.length

def main : IO Unit := IO.interact $ λ input ↦
  let board := Board.parse input
  s!"Part1: {board.numVisited}\n" ++
  s!"Part2: {board.countStuckInLoopObstacles}\n"
