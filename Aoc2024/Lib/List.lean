def List.first2! [Inhabited α] : List α → α × α
| x :: y :: _ => (x, y)
| _ => panic "List.first2!: list does not contain 2 elements"
