let formulae: array<Formula.formula> = [
  Parser.parse("and(A, next(or(B, C)))"),
]
let trace = list{
  Trace.stateOf([]),
  Trace.stateOf([]),
  Trace.stateOf([]),
  Trace.stateOf([]),
}