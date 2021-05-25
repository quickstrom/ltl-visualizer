let formulae: array<Formula.formula> = [
  Parser.parse("A"),
  Parser.parse("or(A, B, C)"),
  Parser.parse("next(B)"),
  Parser.parse("eventually(or(A, B))"),
]
let trace = list{
  Trace.stateOf([]),
  Trace.stateOf([]),
  Trace.stateOf([]),
  Trace.stateOf([]),
}