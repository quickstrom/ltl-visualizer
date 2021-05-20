let formulae: array<Formula.formula> = [
  Parser.parse("and(A, next(or(B, C)))"),
]
let trace = list{
  Trace.state_of([]),
  Trace.state_of([]),
  Trace.state_of([]),
  Trace.state_of([]),
}