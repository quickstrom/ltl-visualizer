let formula1 = Parser.parse("or(A, and(B, next(C)))")
let formula2 = Parser.parse("and(A, next(B))")
let trace = list{Trace.state_of(['A']), Trace.state_of(['B', 'C']), Trace.state_of(['A'])}