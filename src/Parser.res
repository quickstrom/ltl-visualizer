exception ParseError(string)

module Syntax = {
  let not = p => Formula.Not(p)
  let \"and" = (p, q) => Formula.And(p, q)
  let or = (p, q) => Formula.Or(p, q)
  let next = p => Formula.Next(p)
  let always = p => Formula.Always(p)
  let eventually = p => Formula.Eventually(p)
  let until = (p, q) => Formula.Until(p, q)
}

// Atomic propositions: A to Z
let atomics: array<(string, Formula.formula)> = Array.map(n => {
  let c = Char.chr(n)
  (String.make(1, c), Formula.Atomic(c))
}, Belt.Array.range(Char.code('A'), Char.code('Z')))

let parseUnsafe: string => Formula.formula = %raw(`
  function(str) {
    let keys = [];
    let values = [];

    Object.entries(Syntax).concat(atomics).forEach(([key, value]) => {
        keys.push(key);
        values.push(value);
    });

    const f = new Function(keys, "return " + str + ";");

    const formula = f.apply(null, values);
    if (formula === null || formula === undefined) {
      throw Error("Got null or undefined");
    } else if (typeof formula === "object" && formula.hasOwnProperty("TAG")) {
      return formula;
    } else if (typeof formula === "function") {
      throw Error(formula.name.concat(" is not applied"));
    } else {
      throw Error("Got ".concat(formula.toString()));
    }
  }
`)

let parse: string => Formula.formula = s =>
  try {
    parseUnsafe(s)
  } catch {
  | Js.Exn.Error(obj) =>
    switch Js.Exn.message(obj) {
    | Some(m) => raise(ParseError("Parse error: " ++ m))
    | None => raise(ParseError("Parse error"))
    }
  }