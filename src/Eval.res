exception EmptyTrace

let rec eval: (Formula.formula, Trace.trace) => bool = (f, trace) =>
  switch (f, trace) {
  | (Atomic(c), list{state, ..._}) => state->Belt.Set.has(c)
  | (Atomic(_), list{}) => raise(EmptyTrace)
  | (Not(p), _) => !eval(p, trace)
  | (And(p, q), _) => eval(p, trace) && eval(q, trace)
  | (Or(p, q), _) => eval(p, trace) || eval(q, trace)
  | (Next(p), list{_, ...rest}) => eval(p, rest)
  | (Next(_), list{}) => raise(EmptyTrace)
  }
