import Aoc2024.Lib.Utils
import Std.Data.HashSet.Basic
import Batteries.Data.HashMap.Basic

structure File where
  id : ℕ
  space : ℕ
  free : ℕ
deriving Inhabited

abbrev Disk := List File

def parseInputRec : ℕ → List ℕ → Disk
  | _, [] => []
  | id, x :: [] => [ {  id := id, space := x, free := 0 } ]
  | id, x :: y :: xs => [ { id := id, space := x, free := y } ] ++ parseInputRec (id + 1) xs

def parseInput (input : String) : Disk :=
  parseInputRec 0 $ input.stripSuffix "\n" |>.toList |>.map (Char.toNat · - Char.toNat '0')

partial def expandAndCompact (disk : Disk) : List ℕ :=
  match disk with
  | [] => []
  | { id := _, space := 0, free := 0} :: xs => expandAndCompact xs
  | { id := _, space := 0, free := free} :: xs =>
    match xs.getLast? with
    | none => []
    | some last =>
      if last.space <= free then
        List.replicate last.space last.id ++ (expandAndCompact $ [{id := last.id, space := 0, free := free - last.space}] ++ xs.dropLast)
      else
        List.replicate free last.id ++ (expandAndCompact $ xs.dropLast ++ [{last with space := last.space - free }])
  | { id := id, space := space, free := free} :: xs => List.replicate space id ++ (expandAndCompact $ [{id := id, space := 0, free := free}] ++ xs)

def checksum (list : List ℕ) : ℕ :=
  list.enum.map (λ ⟨i, x⟩ => i * x) |>.sum

structure Block where
  files : List File
  free : ℕ

abbrev Blocks := List Block

def Block.removeFirstFile (block : Block) : Block :=
  match block.files with
  | [] => block
  | x :: xs => { block with files := [{ x with id := 0 }] ++ xs, free := block.free }

def File.toBlock (file : File) : Block :=
  { files := [{file with free := 0}], free := file.free }

def Disk.toBlocks (disk : Disk) : Blocks :=
  disk.map File.toBlock

inductive Result (α : Type)
  | compacted : α → Result α
  | same : α → Result α

def Result.map {α β} (f : α → β) : Result α → Result β
  | Result.compacted x => Result.compacted $ f x
  | Result.same x => Result.same $ f x

def tryCompactOne (blocks : Blocks) : Result Blocks :=
  match blocks with
  | [] => Result.same blocks
  | x :: xs =>
    match xs.getLast? with
    | none => Result.same blocks
    | some last =>
      match last.files with
      | file :: _ =>
        if file.id > 0 && file.space <= x.free then
          Result.compacted $ [{ files := x.files ++ [file], free := x.free - file.space }] ++ xs.dropLast ++ [last.removeFirstFile]
        else
          tryCompactOne xs |>.map (x :: ·)
      | _ =>
          tryCompactOne xs |>.map (x :: ·)

partial def Blocks.compactBlocks (blocks : Blocks) : Blocks :=
  match tryCompactOne blocks with
  | Result.same blocks => match blocks.getLast? with
    | none => []
    | some last => Blocks.compactBlocks blocks.dropLast ++ [last]
  | Result.compacted blocks => compactBlocks blocks

partial def Blocks.compactBlocksOne (blocks : Blocks) : Blocks :=
  match tryCompactOne blocks with
  | Result.same blocks => match blocks.getLast? with
    | none => []
    | some last => Blocks.compactBlocksOne blocks.dropLast ++ [last]
  | Result.compacted blocks => blocks

def Block.expand (block : Block) : List ℕ :=
  block.files.flatMap (λ file => List.replicate file.space file.id) ++ List.replicate block.free 0

def Blocks.expand (blocks : Blocks) : List ℕ :=
  blocks.flatMap Block.expand

def solve1 (disk : Disk) : ℕ :=
  checksum $ expandAndCompact disk

def solve2 (disk : Disk) : ℕ :=
  checksum $ disk.toBlocks |>.compactBlocks |>.expand

def main : IO Unit := IO.interact $ λ input ↦
  let disk := parseInput input
  s!"Part1: {solve1 disk}\n" ++
  s!"Part2: {solve2 disk}\n"
