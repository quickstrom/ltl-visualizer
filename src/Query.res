type query = {"formulae": option<array<Formula.formula>>, "trace": option<list<Trace.state>>}

let parse: (option<string>, option<string>) => query = (formulae, trace) =>
  {
    "formulae": Js.Option.map(
      (. s) => Belt.List.toArray(List.map(Parser.parse, String.split_on_char('|', s))),
      formulae,
    ),
    "trace": Js.Option.map(
      (. s) => List.map(Trace.stateOfString, String.split_on_char('|', s)),
      trace,
    ),
  }

let render: query => (string, string) = query => (
  Monoid.StringJoin.joinArray(
    "|",
    Array.map(Formula.print_formula, Js.Option.getWithDefault([], query["formulae"])),
  ),
  Monoid.StringJoin.joinArray(
    "|",
    Array.map(Trace.stateToString, Belt.List.toArray(Js.Option.getWithDefault(list{}, query["trace"]))),
  ),
)

let get: unit => query = %raw(`
    function() {
      var searchParams = new URLSearchParams(window.location.search);
      return parse(searchParams.get("formulae") || undefined, searchParams.get("trace") || undefined);
    }
  `)

let renderURL: query => string = %raw(`
    function (query) {
      var searchParams = new URLSearchParams(window.location.search);
      var [formula, trace] = render(query)
      searchParams.set("formulae", formula)
      searchParams.set("trace", trace)
      return window.location.origin + "?" + searchParams.toString();
    }
  `)
