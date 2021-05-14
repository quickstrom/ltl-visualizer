exception EmptyTrace

type strength =
  | True
  | False

type rec value =
  | Residual(residual)
  | Pure(bool)
and residual =
  | Conjunction(residual, residual)
  | Disjunction(residual, residual)
  | Next(Formula.formula, value)

let rec negateResidual: residual => residual = r =>
  switch r {
  | Conjunction(p, q) => Disjunction(negateResidual(p), negateResidual(q))
  | Disjunction(p, q) => Conjunction(negateResidual(p), negateResidual(q))
  | Next(f, r) => Next(Formula.Not(f), r)
  }

let negateValue: value => value = v =>
  switch v {
  | Residual(r) => Residual(negateResidual(r))
  | Pure(v) => Pure(!v)
  }

let evalAnd: (value, value) => value = (p, q) =>
  switch (p, q) {
  | (Pure(true), p) => p
  | (p, Pure(true)) => p
  | (Pure(false), _) => Pure(false)
  | (_, Pure(false)) => Pure(false)
  | (Residual(r1), Residual(r2)) => Residual(Conjunction(r1, r2))
  }

let evalOr: (value, value) => value = (p, q) =>
  switch (p, q) {
  | (Pure(true), _) => Pure(true)
  | (_, Pure(true)) => Pure(true)
  | (Pure(false), p) => p
  | (p, Pure(false)) => p
  | (Residual(r1), Residual(r2)) => Residual(Disjunction(r1, r2))
  }

let rec eval: (Formula.formula, Trace.state) => value = (f, state) =>
  switch f {
  | Top => Pure(true)
  | Bottom => Pure(true)
  | Atomic(c) => Pure(state->Belt.Set.has(c))
  | Not(p) => negateValue(eval(p, state))
  | And(p, q) => evalAnd(eval(p, state), eval(q, state))
  | Or(p, q) => evalOr(eval(p, state), eval(q, state))
  | Next(p) => Residual(Next(p, eval(p, state)))
  | Always(p) => eval(Formula.Not(Formula.Until(Formula.Top, Formula.Not(p))), state)
  | Until(p, q) => {
      let cont: residual = Next(Until(p, q), eval(p, state))
      switch (eval(p, state), eval(q, state)) {
      | (_, Pure(true)) => Pure(true)
      | (Pure(true), Pure(false)) => Residual(cont)
      | (Pure(false), q') => q'
      | (Residual(r), Pure(false)) => Residual(Conjunction(r, cont))
      | (Residual(pr), Residual(qr)) => Residual(Disjunction(qr, Conjunction(pr, cont)))
      | (Pure(true), Residual(qr)) => Residual(Disjunction(qr, cont))
      }
    }
  }

let map2: (('a, 'b) => 'c, option<'a>, option<'b>) => option<'c> = (f, oa, ob) =>
  Belt.Option.flatMap(oa, a => Belt.Option.map(ob, b => f(a, b)))

module EvalTrace = {
  let rec step: (residual, Trace.state) => value = (r, state) =>
    switch r {
    | Conjunction(r1, r2) => evalAnd(step(r1, state), step(r2, state))
    | Disjunction(r1, r2) => evalOr(step(r1, state), step(r2, state))
    | Next(f, _) => eval(f, state)
    }

  let rec stop: residual => option<bool> = r =>
    switch r {
    | Conjunction(r1, r2) => map2((a, b) => a && b, stop(r1), stop(r2))
    | Disjunction(r1, r2) => map2((a, b) => a || b, stop(r1), stop(r2))
    | Next(_, Pure(b)) => Some(b)
    | Next(_, Residual(_)) => None
    }

  let rec loopLast: (value, Trace.state) => bool = (value, last) =>
    switch value {
    | Pure(r) => r
    | Residual(r) =>
      switch stop(r) {
      | Some(result) => result
      | None => loopLast(step(r, last), last)
      }
    }

  let rec stepStates: (value, Trace.trace) => bool = (value, trace) =>
    switch trace {
    | list{} => raise(EmptyTrace)
    | list{last} =>
      switch value {
      | Pure(r) => r
      | Residual(r) => loopLast(step(r, last), last)
      }
    | list{current, ...rest} =>
      switch value {
      | Pure(r) => r
      | Residual(r) => stepStates(step(r, current), rest)
      }
    }

  let evalTrace: (Formula.formula, Trace.trace) => bool = (f, trace) => {
    switch trace {
    | list{} => raise(EmptyTrace)
    | list{last} => loopLast(eval(f, last), last)
    | list{first, ...rest} => stepStates(eval(f, first), rest)
    }
  }
}

let rec evalAll: (Formula.formula, Trace.trace) => list<bool> = (f, t) =>
  switch t {
  | list{} => list{}
  | list{_, ...rest} => list{EvalTrace.evalTrace(f, t), ...evalAll(f, rest)}
  }
