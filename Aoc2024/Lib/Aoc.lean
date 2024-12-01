def lines (s : String) : List String := s.splitOn "\n" |>.reverse |>.dropWhile String.isEmpty |>.reverse

def Prod.tolist {α} (p : α × α) : List α := [p.1, p.2]

def uncurry (f : α → β → γ) : (α × β → γ) | (a, b) => f a b
