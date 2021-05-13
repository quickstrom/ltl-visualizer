module Syntax = {
  let not = p => Formula.Not(p)
  let \"and" = (p, q) => Formula.And(p, q)
  let or = (p, q) => Formula.Or(p, q)
  let next = p => Formula.Next(p)
  let always = p => Formula.Always(p)
  let until = (p, q) => Formula.Until(p, q)
}

// Atomic propositions: A to Z
let atomics: array<(string, Formula.formula)> = Array.map(n => {
  let c = Char.chr(n)
  (String.make(1, c), Formula.Atomic(c))
}, Belt.Array.range(Char.code('A'), Char.code('Z')))

let parse = %raw(`
  function(str) {
    let keys = [];
    let values = [];

    Object.entries(Syntax).concat(atomics).forEach(([key, value]) => {
        keys.push(key);
        values.push(value);
    });

    const f = new Function(keys, "return " + str + ";");

    return f.apply(null, values);
  }
`)
