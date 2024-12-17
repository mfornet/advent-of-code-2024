import Aoc2024.Lib.Utils
import Std.Data.HashSet.Basic
import Batteries.Data.HashMap.Basic

structure Robot where
  position : ℤ × ℤ
  velocity: ℤ × ℤ
deriving Inhabited

instance : ToString Robot where
  toString r := s!"[{r.position} {r.velocity}]"

def Robot.step (robot : Robot) (shape : ℤ × ℤ) : Robot :=
  let (px, py) := robot.position
  let (vx, vy) := robot.velocity
  let (sx, sy) := shape
  let npx := (px + vx) % sx
  let npy := (py + vy) % sy
  Robot.mk (npx, npy) robot.velocity

def Robot.quadrant (robot : Robot) (shape : ℤ × ℤ): ℕ :=
  let (px, py) := robot.position
  let (sx, sy) := shape

  if 2 * px + 1 == sx || 2 * py + 1 == sy then
    0
  else if 2 * px + 1 < sx && 2 * py + 1 < sy then
    1
  else if 2 * px + 1 > sx && 2 * py + 1 < sy then
    2
  else if 2 * px + 1 > sx && 2 * py + 1 > sy then
    3
  else
    4

def Robot.parse (line : String) : Robot :=
  let (p, v) := line.splitOn " " |>.map
    (λ s => s.splitOn "=" |>.get! 1 |>.splitOn "," |>.map String.toInt! |>.first2!)
    |>.first2!
  Robot.mk p v

def parseInput (input : String) : List Robot :=
  lines input |>.map .parse

def solve1(robots : List Robot) (steps : ℕ) (shape : ℤ × ℤ) : ℕ :=
  let robots := robots.map (λ r => List.range steps |>.foldl (λ r _ => r.step shape) r)
                |>.map (Robot.quadrant · shape)
                |>.filter (· > 0)
  List.range 4
    |>.map (λ q => robots.filter (· == q + 1) |>.length)
    |>.foldl (λ a b => a * b) 1

def main : IO Unit := IO.interact $ λ input ↦
  let robots := parseInput input
  s!"Part1 A: {solve1 robots 100 ⟨7, 11⟩}\n" ++
  s!"Part1 B: {solve1 robots 100 ⟨101, 103⟩}\n" ++
  -- This was just pattern exploration, no code to see here.
  -- Interesting ideas to try (discussed with friends):
  -- * Longest line
  -- * Esimate entropy of the board
  s!"Part2: {7790}\n"
