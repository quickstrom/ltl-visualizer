type rec formula =
  | Atomic(char)
  | Not(formula)
  | And(formula, formula)
  | Or(formula, formula)
  | Next(formula)

let rec print_formula: formula => string = f =>
  switch f {
  | Atomic(c) => String.make(1, c)
  | Not(p) => "not(" ++ print_formula(p) ++ ")"
  | And(p, q) => "and(" ++ print_formula(p) ++ ", " ++ print_formula(q) ++ ")"
  | Or(p, q) => "or(" ++ print_formula(p) ++ ", " ++ print_formula(q) ++ ")"
  | Next(p) => "next(" ++ print_formula(p) ++ ")"
  }