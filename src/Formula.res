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

module CharCmp = Belt.Id.MakeComparable({
  type t = char
  let cmp = Pervasives.compare
})

type names = Belt.Set.t<char, CharCmp.identity>

let emptyNames: names = Belt.Set.make(~id=module(CharCmp))

let atomicNames: formula => names = f => {
  let rec go: (names, formula) => names = (names, f) =>
    switch f {
    | Atomic(c) => names->Belt.Set.add(c)
    | Not(p) => go(names, p)
    | And(p, q) => Belt.Set.union(go(names, p), go(names, q))
    | Or(p, q) => Belt.Set.union(go(names, p), go(names, q))
    | Next(p) => go(names, p)
    }
  go(emptyNames, f)
}
