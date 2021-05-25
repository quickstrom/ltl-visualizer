let formulae: array<Formula.formula> = [
  Parser.parse("not(and(B, next(eventually(B))))"),
  Parser.parse("and(B, next(eventually(B)))"),
]
let trace = list{
  Trace.stateOf([]),
  Trace.stateOf(['B']),
  Trace.stateOf([]),
  Trace.stateOf([]),
}