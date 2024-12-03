import Aoc2024.Lib.Utils

inductive Step where
  | Initial
  | M
  | MU
  | MUL
  | MULp
  | MULpc
  | D
  | DO
  | DOp
  | DON
  | DON'
  | DON'T
  | DON'Tp


structure State (check_enabled: Bool) where
  sum: ℕ
  left: ℕ
  right: ℕ
  enabled: Bool
  step: Step

def createState (check_enabled : Bool): State check_enabled :=
  { sum := 0, left := 0, right := 0, enabled := true, step := Step.Initial }

def State.next {check_enabled : Bool} (s : State check_enabled) (c : Char) : State check_enabled :=
  match (⟨s.step, c, s.enabled || ¬check_enabled⟩ : Step × Char × Bool) with
  | ⟨_, 'm', true⟩ => { s with step := Step.M, left := 0, right := 0 }
  -- ignore `mul` pattern if it is disabled
  | ⟨_, 'm', false⟩ => { s with step := Step.Initial }
  | ⟨Step.M, 'u', _⟩ => { s with step := Step.MU }
  | ⟨Step.MU, 'l', _⟩ => { s with step := Step.MUL }
  | ⟨Step.MUL, '(', _⟩ => { s with step := Step.MULp }
  | ⟨Step.MULp, _, _⟩ => match c with
    | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => { s with right := s.right * 10 + c.toNat - '0'.toNat }
    | ',' => { s with step := Step.MULpc }
    | _ => { s with step := Step.Initial }
  | ⟨Step.MULpc, _, _⟩ => match c with
    | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => { s with left := s.left * 10 + c.toNat - '0'.toNat }
    | ')' => { s with step := Step.Initial, sum := s.sum + s.left * s.right }
    | _ => { s with step := Step.Initial }
  -- Check for the patterns do() and don't()
  | ⟨_, 'd', _⟩ => { s with step := Step.D }
  | ⟨Step.D, 'o', _⟩ => { s with step := Step.DO }
  | ⟨Step.DO, '(', _⟩ => { s with step := Step.DOp }
  | ⟨Step.DOp, ')', _⟩ => { s with step := Step.Initial, enabled := true }
  | ⟨Step.DO, 'n', _⟩ => { s with step := Step.DON }
  | ⟨Step.DON, '\'', _⟩ => { s with step := Step.DON' }
  | ⟨Step.DON', 't', _⟩ => { s with step := Step.DON'T }
  | ⟨Step.DON'T, '(', _⟩ => { s with step := Step.DON'Tp }
  | ⟨Step.DON'Tp, ')', _⟩ => { s with step := Step.Initial, enabled := false }
  | _ => { s with step := Step.Initial }

def solve (check_enabled : Bool) (input : String) : ℕ :=
  input.toList.foldl State.next (createState check_enabled) |>.sum

def main : IO Unit := IO.interact $ λ input ↦
  s!"Part1: {solve false input}\n" ++
  s!"Part2: {solve true input}\n"
