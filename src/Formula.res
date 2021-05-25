type rec formula =
  | Top
  | Bottom
  | Atomic(char)
  | Not(formula)
  | And(array<formula>)
  | Or(array<formula>)
  | Implies(formula, formula)
  | Next(formula)
  | Always(formula)
  | Eventually(formula)
  | Until(formula, formula)

let rec print_formula: formula => string = f =>
  switch f {
  | Top => "top"
  | Bottom => "bottom"
  | Atomic(c) => String.make(1, c)
  | Not(p) => "not(" ++ print_formula(p) ++ ")"
  | And(ps) => "and(" ++ Monoid.StringJoin.joinArray(Monoid.String.make(", "), Belt.Array.map(ps, print_formula)) ++ ")"
  | Or(ps) => "or(" ++ Monoid.StringJoin.joinArray(Monoid.String.make(", "), Belt.Array.map(ps, print_formula)) ++ ")"
  | Implies(p, q) => "implies(" ++ print_formula(p) ++ ", " ++ print_formula(q) ++ ")"
  | Next(p) => "next(" ++ print_formula(p) ++ ")"
  | Always(p) => "always(" ++ print_formula(p) ++ ")"
  | Eventually(p) => "eventually(" ++ print_formula(p) ++ ")"
  | Until(p, q) => "until(" ++ print_formula(p) ++ ", " ++ print_formula(q) ++ ")"
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
    | Top => emptyNames
    | Bottom => emptyNames
    | Atomic(c) => names->Belt.Set.add(c)
    | Not(p) => go(names, p)
    | And(ps) => Array.fold_left((a, b) => Belt.Set.union(go(names, b), a), emptyNames, ps)
    | Or(ps) => Array.fold_left((a, b) => Belt.Set.union(go(names, b), a), emptyNames, ps)
    | Implies(p, q) => Belt.Set.union(go(names, p), go(names, q))
    | Next(p) => go(names, p)
    | Always(p) => go(names, p)
    | Eventually(p) => go(names, p)
    | Until(p, q) => Belt.Set.union(go(names, p), go(names, q))
    }
  go(emptyNames, f)
}
