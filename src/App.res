module TraceStates = {
  @react.component
  let make = (~formula, ~trace) => {
    let states = Belt.Array.mapWithIndex(Belt.List.toArray(Eval.evalAll(formula, trace)), (
      i,
      result,
    ) => {
      <li key={string_of_int(i)}> <input type_="checkbox" disabled=true checked=result /> </li>
    })
    <ul className="states"> {React.array(states)} </ul>
  }
}

module TraceVisualizer = {
  @react.component
  let make = (~trace, ~formulae) => {
    let allNames = Array.fold_left(
      (names, f) => Belt.Set.union(names, Formula.atomicNames(f)),
      Formula.emptyNames,
      formulae,
    )

    <div className="trace-visualizer">
      <h2> {React.string("Atomic Propositions")} </h2>
      {React.array(
        Belt.Array.mapWithIndex(Belt.Set.toArray(allNames), (i, name) =>
          <p key={string_of_int(i)}>
            <code className="formula"> {React.string(String.make(1, name))} </code>
            <TraceStates formula=Formula.Atomic(name) trace />
          </p>
        ),
      )}
      <h2> {React.string("Formulae")} </h2>
      {React.array(
        Belt.Array.mapWithIndex(formulae, (i, formula) =>
          <p key={string_of_int(i)}>
            <code className="formula"> {React.string(Formula.print_formula(formula))} </code>
            <TraceStates formula trace />
          </p>
        ),
      )}
    </div>
  }
}

module App = {
  @react.component
  let make = () => {
    <div className="app">
      <header className="header">
        <h1> {React.string("Linear Temporal Logic Visualizer")} </h1>
        <TraceVisualizer trace=Demo.trace formulae=Demo.formulae />
      </header>
    </div>
  }
}

switch ReactDOM.querySelector("#app-root") {
| Some(root) => ReactDOM.render(<App />, root)
| None => ()
}
