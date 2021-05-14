let formulae: array<Formula.formula> = [
  Parser.parse("or(A, and(B, next(C)))"),
  Parser.parse("always(A)"),
  Parser.parse("always(or(A, or(B, D)))"),
  Parser.parse("until(or(A, B), D)"),
  Parser.parse("always(next(A))"),
]
let trace = list{
  Trace.state_of(['A']),
  Trace.state_of(['B', 'C']),
  Trace.state_of(['A']),
  Trace.state_of(['A', 'D']),
}

Js.log(Formula.print_formula(Parser.parse("always(next(A))")))
Js.log(
  Eval.EvalTrace.evalTrace(
    Parser.parse("until(A, D)"),
    list{
      Trace.state_of(['A']),
      Trace.state_of(['A', 'B', 'C']),
      Trace.state_of(['A']),
      Trace.state_of(['D']),
    },
  ),
)
