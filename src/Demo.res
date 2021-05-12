let formula = Parser.parse("and(A, or(B, C))")
let trace = list{Trace.state_of(['B']), Trace.state_of(['A', 'B']), Trace.state_of(['C'])}