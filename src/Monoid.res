module type Semigroup = {
  type t
  let empty: t
  let append: (t, t) => t
}

module Join = (S: Semigroup) => {
  let joinArray = (s: S.t, xs: array<S.t>) =>
    switch Belt.Array.slice(xs, ~len=1, ~offset=0) {
    | [x] => Array.fold_left((a, b) => S.append(a, S.append(s, b)), x, Belt.Array.sliceToEnd(xs, 1))
    | _ => S.empty
    }
}

module String = {
  type t = string
  let empty = ""
  let append = (a, b) => a ++ b
  let make = s => s
}

module Element = {
  type t = React.element
  let empty = <> </>
  let append = (a, b) => <>{a} {b} </>
  let make = s => s
}

module StringJoin = Join(String)

module ElementJoin = Join(Element)
