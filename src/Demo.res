let formulae: array<Formula.formula> = [
  Parser.parse("or(A, and(B, next(C)))"),
  Parser.parse("always(A)"),
  Parser.parse("always(or(A, or(B, D)))"),
  Parser.parse("until(or(A, B), D)"),
  //Parser.parse("always(next(A))"),
]
let trace = list{
  Trace.state_of(['A']),
  Trace.state_of(['B', 'C']),
  Trace.state_of(['A']),
  Trace.state_of(['A', 'D']),
}

let f = Formula.Eventually(Formula.Next(Formula.Atomic('A')))
let s1 = Trace.state_of(['B'])

switch Eval.eval(f, s1) {
| Eval.Pure(b) => Js.log(string_of_bool(b))
| Eval.Residual(r) =>
  switch Eval.EvalTrace.stop(r) {
  | Some(b) => Js.log2("Stopping with", b)
  | None => Js.log2("Must continue with:", Eval.printResidual(r))
  }
}
