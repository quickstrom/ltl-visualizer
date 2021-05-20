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

let rec printValue: value => string = value =>
  switch value {
  | Residual(r) => printResidual(r)
  | Pure(b) => string_of_bool(b)
  }
and printResidual: residual => string = r =>
  switch r {
  | Conjunction(r1, r2) => printResidual(r1) ++ " && " ++ printResidual(r2)
  | Disjunction(r1, r2) => printResidual(r1) ++ " || " ++ printResidual(r2)
  | Next(formula, value) =>
    "next(" ++ Formula.print_formula(formula) ++ ", " ++ printValue(value) ++ ")"
  }

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

let evalAnd: (Lazy.t<value>, Lazy.t<value>) => value = (p, q) =>
  switch Lazy.force(p) {
  | Pure(false) => Pure(false)
  | Pure(true) => Lazy.force(q)
  | Residual(r1) =>
    switch Lazy.force(q) {
    | Pure(false) => Pure(false)
    | Pure(true) => Residual(r1)
    | Residual(r2) => Residual(Conjunction(r1, r2))
    }
  }

let evalOr: (Lazy.t<value>, Lazy.t<value>) => value = (p, q) =>
  switch Lazy.force(p) {
  | Pure(true) => Pure(true)
  | Pure(false) => Lazy.force(q)
  | Residual(r1) =>
    switch Lazy.force(q) {
    | Pure(true) => Pure(true)
    | Pure(false) => Residual(r1)
    | Residual(r2) => Residual(Disjunction(r1, r2))
    }
  }

let rec eval: (Formula.formula, Trace.state) => value = (f, state) =>
  switch f {
  | Top => Pure(true)
  | Bottom => Pure(true)
  | Atomic(c) => Pure(state->Belt.Set.has(c))
  | Not(p) => negateValue(eval(p, state))
  | And(p, q) => evalAnd(lazy eval(p, state), lazy eval(q, state))
  | Or(p, q) => evalOr(lazy eval(p, state), lazy eval(q, state))
  | Next(p) => Residual(Next(p, eval(p, state)))
  | Always(p) =>
    switch eval(p, state) {
    | Pure(false) => Pure(false)
    | Pure(true) => Residual(Next(Always(p), Pure(true)))
    | Residual(r) => Residual(Conjunction(r, Next(Always(p), Residual(r))))
    }
  | Eventually(p) =>
    switch eval(p, state) {
    | Pure(true) => Pure(true)
    | Pure(false) => Residual(Next(Eventually(p), Pure(false)))
    | Residual(r) => Residual(Disjunction(r, Next(Eventually(p), Residual(r))))
    }
  | Until(p, q) => {
      let cont: residual = Next(Until(p, q), eval(q, state))
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
    | Conjunction(r1, r2) => evalAnd(lazy step(r1, state), lazy step(r2, state))
    | Disjunction(r1, r2) => evalOr(lazy step(r1, state), lazy step(r2, state))
    | Next(f, _) => eval(f, state)
    }

  let rec stop: residual => bool = r =>
    switch r {
    | Conjunction(r1, r2) => stop(r1) && stop(r2)
    | Disjunction(r1, r2) => stop(r1) || stop(r2)
    | Next(_, Pure(b)) => b
    | Next(_, Residual(r)) => stop(r)
    }

  let loopLast: (value) => bool = (value) =>
    switch value {
    | Pure(r) => r
    | Residual(r) => stop(r)
    }

  let rec stepStates: (value, Trace.trace) => bool = (value, trace) =>
    switch trace {
    | list{} => raise(EmptyTrace)
    | list{last} =>
      switch value {
      | Pure(r) => r
      | Residual(r) => loopLast(step(r, last))
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
    | list{last} => loopLast(eval(f, last))
    | list{first, ...rest} => stepStates(eval(f, first), rest)
    }
  }
}

let rec evalAll: (Formula.formula, Trace.trace) => list<bool> = (f, t) =>
  switch t {
  | list{} => list{}
  | list{_, ...rest} => list{EvalTrace.evalTrace(f, t), ...evalAll(f, rest)}
  }
