import Aoc2024.Lib.Utils
import Std.Data.HashSet.Basic
import Batteries.Data.HashMap.Basic

structure File where
  id : ℕ
  space : ℕ
deriving Inhabited, Repr

structure Block where
  files : Array File
  free : ℕ
deriving Inhabited, Repr

instance : ToString File where
  toString file := s!"{file.id} {file.space}"

instance : ToString Block where
  toString block := s!"{block.files} {block.free}"

abbrev Disk := Array Block

def parseInputRec : ℕ → List ℕ → Disk
  | _, [] => #[]
  | id, x :: [] => #[Block.mk #[File.mk id x] 0]
  | id, x :: y :: xs => parseInputRec (id + 1) xs |>.push $ Block.mk #[File.mk id x] y

def parseInput (input : String) : Disk :=
  parseInputRec 0 (input.stripSuffix "\n" |>.toList |>.map DigitToNat!) |>.reverse

def File.decrease (file : File) (space : ℕ) : File :=
  { file with space := file.space - space }

def Block.first (block : Block) : File :=
  block.files.get! 0

def Block.add (block : Block) (file : File) : Block :=
  { block with files := block.files.push file, free := block.free - file.space }

structure ChecksumState where
  position : ℕ
  checksum : ℕ

def ChecksumState.advance (state : ChecksumState) (delta : ℕ) : ChecksumState :=
  { state with position := state.position + delta }

def ChecksumState.sum (state : ChecksumState) (delta : ℕ) : ℕ :=
  delta * (delta - 1) / 2 + state.position * delta

def File.checksum (file : File) (state : ChecksumState) : ChecksumState :=
  ChecksumState.mk (state.position + file.space) (state.checksum + file.id * state.sum file.space)

def Block.checksum (block : Block) (state : ChecksumState) : ChecksumState :=
  block.files.foldl (λ state file => file.checksum state) state |>.advance block.free

def checksum (disk : Disk) : ℕ :=
  disk.foldl (λ state block => block.checksum state) (ChecksumState.mk 0 0) |>.checksum

def Block.removeFirstFile (block : Block) : Block :=
  { block with files := block.files.modify 0 (λ file => { file with id := 0 }) }

def compact1' (disk : Disk) : Disk :=
  List.range disk.size |>.reverse.foldl (λ disk i =>
    let file := disk.back!.first
    let disk := disk.pop
    let ⟨disk, file⟩ := List.range i |>.foldl (λ (disk, file) j =>
      if file.space == 0 then
        ⟨disk, file⟩
      else
        let block := disk.get! j
        if block.free > 0 then
          let used := block.free.min file.space
          let disk := disk.modify j (·.add $ File.mk file.id used)
          let file := file.decrease used
          ⟨disk, file⟩
        else
          ⟨disk, file⟩
    ) (⟨disk, file⟩ : Disk × File)

    if file.space > 0 then
      disk.push $ Block.mk #[file] 0
    else
      disk
  ) disk

def compact2 (disk : Disk) : Disk :=
  List.range disk.size |>.reverse.foldl (λ disk i =>
    let required := disk.get! i |>.first.space
    match List.range i |>.find? (λ j => (disk.get! j |>.free) >= required) with
    | none => disk
    | some j =>
      let file := disk.get! i |>.first
      let disk := disk.modify i (·.removeFirstFile)
      disk.modify j (·.add file)
  ) disk

def solve (disk : Disk) (compact : Disk → Disk) : ℕ :=
  checksum $ compact disk

def main : IO Unit := IO.interact $ λ input ↦
  let disk := parseInput input
  s!"Part1: {solve disk compact1'}\n" ++
  s!"Part2: {solve disk compact2}\n"
