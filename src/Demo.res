let formulae: array<Formula.formula> = [
  Parser.parse("or(A, and(B, next(C)))"),
  Parser.parse("always(A)"),
  Parser.parse("always(or(A, B))"),
]
let trace = list{Trace.state_of(['A']), Trace.state_of(['B', 'C']), Trace.state_of(['A'])}
