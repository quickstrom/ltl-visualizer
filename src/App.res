let value: Dom.element => string = %raw(`
  function(obj) { return obj.value }
`)

let clear: Dom.element => unit = %raw(`
  function(obj) { obj.value = ""; }
`)

let rec prettyPrint: Formula.formula => React.element = f => {
  let literal = s => <span className="literal"> {React.string(s)} </span>
  let atomic = c => <span className="atomic"> {React.string(String.make(1, c))} </span>

  module Op = {
    @react.component
    let make = (~name, ~sub: array<Formula.formula>) =>
      <span className="application">
        <span className="operator"> {React.string(name)} </span>
        {React.string("(")}
        {Monoid.ElementJoin.joinArray(React.string(", "), Array.map(prettyPrint, sub))}
        {React.string(")")}
      </span>
  }

  switch f {
  | Top => literal("top")
  | Bottom => literal("bottom")
  | Atomic(c) => atomic(c)
  | Not(p) => <Op name="not" sub={[p]} />
  | And(ps) => <Op name="and" sub={ps} />
  | Or(ps) => <Op name="or" sub={ps} />
  | Implies(p, q) => <Op name="implies" sub={[p, q]} />
  | Next(p) => <Op name="next" sub={[p]} />
  | Always(p) => <Op name="always" sub={[p]} />
  | Eventually(p) => <Op name="eventually" sub={[p]} />
  | Until(p, q) => <Op name="until" sub={[p, q]} />
  }
}

module TraceStates = {
  @react.component
  let make = (~formula, ~trace, ~onToggle: option<(int, bool) => unit>=?) => {
    let onChange = (i, event) => {
      let value = ReactEvent.Form.target(event)["checked"]
      switch onToggle {
      | Some(f) => f(i, value)
      | None => ()
      }
    }
    let states = Belt.Array.mapWithIndex(Belt.List.toArray(Eval.evalAll(formula, trace)), (
      i,
      result,
    ) => {
      let id = Formula.print_formula(formula) ++ string_of_int(i)
      <td key={string_of_int(i)} className="state">
        <input
          id
          type_="checkbox"
          disabled={!Js.Option.isSome(onToggle)}
          checked=result
          onChange={e => onChange(i, e)}
        />
        <label htmlFor=id />
      </td>
    })
    React.array(states)
  }
}

module TraceHeader = {
  @react.component
  let make = (~title, ~trace, ~children: React.element) => {
    let lastIndex = List.length(trace) - 1
    let stateHeaders = Array.mapi((i, _) => {
      let label = if i == lastIndex {
        React.string(string_of_int(i) ++ `..∞`)
      } else {
        React.int(i)
      }
      <th> {React.string("S")} <sub> {label} </sub> </th>
    }, Belt.List.toArray(trace))

    <tr>
      <th> {React.string(title)} </th>
      {React.array(stateHeaders)}
      <th className="actions"> {children} </th>
    </tr>
  }
}

module SyntaxHelp = {
  type entry = {concept: string, description: string, examples: array<Formula.formula>}

  let entries: array<entry> = [
    {
      concept: "Atomic propositions",
      description: "Symbolic names (uppercase letters) that are either true or false in a state.",
      examples: [Formula.Atomic('A'), Formula.Atomic('B')],
    },
    {
      concept: "Logical connectives",
      description: "Operators from propositional logic.",
      examples: [
        Formula.Not(Formula.Atomic('A')),
        Formula.And([Formula.Atomic('A'), Formula.Atomic('B')]),
        Formula.Or([Formula.Atomic('A'), Formula.Atomic('B'), Formula.Atomic('C')]),
        Formula.Implies(Formula.Atomic('A'), Formula.Atomic('B')),
      ],
    },
    {
      concept: "Temporal operators",
      description: "Operators that deal with time.",
      examples: [
        Formula.Next(Formula.Atomic('A')),
        Formula.Eventually(Formula.Atomic('B')),
        Formula.Always(Formula.Atomic('C')),
        Formula.Until(Formula.Atomic('A'), Formula.Atomic('B')),
      ],
    },
  ]

  @react.component
  let make = () => {
    let renderEntry = entry => {
      <tr>
        <td className="concept"> {React.string(entry.concept)} </td>
        <td> {React.string(entry.description)} </td>
        <td className="examples">
          <ul>
            {React.array(
              Array.map(
                example => <li> <code> {prettyPrint(example)} </code> </li>,
                entry.examples,
              ),
            )}
          </ul>
        </td>
      </tr>
    }
    <details>
      <summary> {React.string("Syntax Cheatsheet")} </summary>
      <table>
        <thead>
          <tr>
            <th> {React.string("Concept")} </th>
            <th> {React.string("Description")} </th>
            <th> {React.string("Examples")} </th>
          </tr>
        </thead>
        {React.array(Array.map(renderEntry, entries))}
      </table>
    </details>
  }
}

module TraceVisualizer = {
  @react.component
  let make = (~initialTrace, ~initialFormulae) => {
    let textInput = React.useRef(Js.Nullable.null)
    let (trace, setTrace) = React.useState(_ => initialTrace)
    let (formulae, setFormulae) = React.useState(_ => initialFormulae)
    let (errorMessage, setErrorMessage) = React.useState(_ => None)

    let allNames = Array.fold_left(
      (names, f) => Belt.Set.union(names, Formula.atomicNames(f)),
      Formula.emptyNames,
      formulae,
    )

    let onNewFormula = event => {
      ReactEvent.Form.preventDefault(event)
      // let form = ReactEvent.Form.target(event)
      switch textInput.current->Js.Nullable.toOption {
      | Some(input) =>
        switch input->value->Parser.parse {
        | formula => {
            setFormulae(Js.Array.concat([formula]))
            setErrorMessage(_ => None)
            clear(input)
          }
        | exception Parser.ParseError(s) => setErrorMessage(_ => Some(s))
        }
      | None => ()
      }
    }

    let removeFormula: int => unit = i => {
      setFormulae(fs => Js.Array.filteri((_, i') => i != i', fs))
    }

    let share: (~title: string, ~url: string) => unit = %raw(`
      function (title, url) {
        if (Navigator.share) {
          Navigator.share({ title, url });
        } else if (navigator.clipboard.writeText) {
          navigator.clipboard.writeText(url).then(() => {
            alert("URL copied!");
          }).catch(err => {
            console.error("Copy-to-clipboard with sharable URL failed:", err);
            alert("Couldn't copy the sharable URL to clipboard. Navigating to the URL as a fallback...");
            window.location = url;
          });
        }
      }
    `)

    let formulaTable =
      <div className="trace-visualizer">
        <nav>
          <button
            className="btn btn-primary"
            onClick={_ => {
              let url = Query.renderURL({"formulae": Some(formulae), "trace": Some(trace)})
              share(~title="Linear Temporal Logic Visualizer", ~url)
            }}>
            {React.string("Share")}
          </button>
        </nav>
        <table>
          <TraceHeader trace title="Atomic Proposition">
            <button
              className="btn btn-small"
              onClick={_ => setTrace(t => Belt.List.concat(t, list{Trace.stateOf([])}))}>
              {React.string("+")}
            </button>
          </TraceHeader>
          {React.array(
            Belt.Array.mapWithIndex(Belt.Set.toArray(allNames), (i, name) => {
              let onToggle: (int, bool) => unit = (stateIndex, enabled) => {
                setTrace(Trace.setTraceState(enabled, name, stateIndex))
              }
              <tr key={string_of_int(i)}>
                <td className="formula"> <code> {prettyPrint(Formula.Atomic(name))} </code> </td>
                <TraceStates formula=Formula.Atomic(name) trace onToggle />
                <td className="actions" />
              </tr>
            }),
          )}
          <TraceHeader trace title="Formula"> {<> </>} </TraceHeader>
          {React.array(
            Belt.Array.mapWithIndex(formulae, (i, formula) =>
              <tr key={string_of_int(i)}>
                <td className="formula"> <code> {prettyPrint(formula)} </code> </td>
                <TraceStates formula trace />
                <td className="actions">
                  <button className="btn btn-small" onClick={_ => removeFormula(i)}>
                    {React.string("Remove")}
                  </button>
                </td>
              </tr>
            ),
          )}
        </table>
      </div>

    <>
      {if Js.Array.length(formulae) > 0 {
        formulaTable
      } else {
        React.string("")
      }}
      <form onSubmit=onNewFormula>
        <input
          ref={ReactDOM.Ref.domRef(textInput)}
          className="new-formula"
          autoFocus=true
          placeholder="Enter a new formula..."
        />
        <p className="error-message">
          {switch errorMessage {
          | Some(msg) => React.string(msg)
          | None => React.string("")
          }}
        </p>
        <SyntaxHelp />
      </form>
    </>
  }
}

module App = {
  @react.component
  let make = () => {
    let content = switch (
      Js.Option.getWithDefault(Demo.formulae, Query.get()["formulae"]),
      Js.Option.getWithDefault(Demo.trace, Query.get()["trace"]),
    ) {
    | exception Parser.ParseError(s) => <p className="error-message"> {React.string(s)} </p>
    | (initialFormulae, initialTrace) => <TraceVisualizer initialTrace initialFormulae />
    }

    <div className="app">
      <header className="header">
        <h1> {React.string("Linear Temporal Logic Visualizer")} </h1>
      </header>
      <main> {content} </main>
      <footer>
        <a
          className="btn help"
          href="https://github.com/quickstrom/ltl-visualizer/blob/main/README.md#usage">
          <span className="icon"> {React.string(`🤔`)} </span> {React.string(`Usage`)}
        </a>
        <iframe
          src="https://github.com/sponsors/owickstrom/button"
          title="Sponsor owickstrom"
          height="35"
          width="116"
          id="sponsor-link"
        />
      </footer>
    </div>
  }
}

switch ReactDOM.querySelector("#app-root") {
| Some(root) => ReactDOM.render(<App />, root)
| None => ()
}
