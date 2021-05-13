exception EmptyTrace

module Residual = {
  type strength =
    | Demand
    | True
    | False

  type rec t =
    | Conjunction(t, t)
    | Disjunction(t, t)
    | Next(Formula.formula, strength)

  let rec negate: t => t = r =>
    switch r {
    | Conjunction(p, q) => Disjunction(negate(p), negate(q))
    | Disjunction(p, q) => Conjunction(negate(p), negate(q))
    | Next(f, r) => Next(Formula.Not(f), r)
    }
}

module Value = {
  type t =
    | Residual(Residual.t)
    | Pure(bool)

  let negate: t => t = r =>
    switch r {
    | Residual(r) => Residual(Residual.negate(r))
    | Pure(r) => Pure(!r)
    }
}

let evalAnd: (Value.t, Value.t) => Value.t = (p, q) =>
  switch (p, q) {
  | (Pure(true), p) => p
  | (p, Pure(true)) => p
  | (Pure(false), _) => Pure(false)
  | (_, Pure(false)) => Pure(false)
  | (Residual(r1), Residual(r2)) => Residual(Conjunction(r1, r2))
  }

let evalOr: (Value.t, Value.t) => Value.t = (p, q) =>
  switch (p, q) {
  | (Pure(true), _) => Pure(true)
  | (_, Pure(true)) => Pure(true)
  | (Pure(false), p) => p
  | (p, Pure(false)) => p
  | (Residual(r1), Residual(r2)) => Residual(Disjunction(r1, r2))
  }

let rec eval: (Formula.formula, Trace.state) => Value.t = (f, state) =>
  switch f {
  | Atomic(c) => Pure(state->Belt.Set.has(c))
  | Not(p) => Value.negate(eval(p, state))
  | And(p, q) => evalAnd(eval(p, state), eval(q, state))
  | Or(p, q) => evalOr(eval(p, state), eval(q, state))
  | Next(p) => Residual(Next(p, Demand))
  | Always(p) =>
    switch eval(p, state) {
    | Pure(false) => Pure(false)
    | Pure(true) => Residual(Next(Always(p), True))
    | Residual(r) => Residual(Conjunction(r, Next(Always(p), True)))
    }
  }

let map2: (('a, 'b) => 'c, option<'a>, option<'b>) => option<'c> = (f, oa, ob) =>
  Belt.Option.flatMap(oa, a => Belt.Option.map(ob, b => f(a, b)))

module EvalTrace = {
  let rec step: (Residual.t, Trace.state) => Value.t = (r, state) =>
    switch r {
    | Conjunction(r1, r2) => evalAnd(step(r1, state), step(r2, state))
    | Disjunction(r1, r2) => evalOr(step(r1, state), step(r2, state))
    | Next(f, _) => eval(f, state)
    }

  let rec stop: Residual.t => option<bool> = r =>
    switch r {
    | Conjunction(r1, r2) => map2((a, b) => a && b, stop(r1), stop(r2))
    | Disjunction(r1, r2) => map2((a, b) => a || b, stop(r1), stop(r2))
    | Next(_, True) => Some(true)
    | Next(_, False) => Some(false)
    | Next(_, Demand) => None
    }

  let rec loopLast: (Value.t, Trace.state) => bool = (value, last) =>
    switch value {
    | Pure(r) => r
    | Residual(r) =>
      switch stop(r) {
      | Some(result) => result
      | None => loopLast(step(r, last), last)
      }
    }

  let rec stepStates: (Value.t, Trace.trace) => bool = (value, trace) =>
    switch trace {
    | list{} => raise(EmptyTrace)
    | list{last} => loopLast(value, last)
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
