module Test = {
  let formula = Parser.parse("and(next(A), or(B, C))")
  let trace = list{Trace.state_of(['B']), Trace.state_of(['A', 'B']), Trace.state_of(['C'])}

  let run = () => {
    Js.log(
      Formula.print_formula(formula) ++
      " is valid with " ++
      Trace.print_trace(trace) ++
      ": " ++
      string_of_bool(Eval.eval(formula, trace)),
    )
  }
}

Test.run()
