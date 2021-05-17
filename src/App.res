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
      <li key={string_of_int(i)}>
        <input type_="checkbox" disabled={!Js.Option.isSome(onToggle)} checked=result onChange={e => onChange(i, e)} />
      </li>
    })
    <ul className="states"> {React.array(states)} </ul>
  }
}

module TraceVisualizer = {
  @react.component
  let make = (~initialTrace, ~formulae) => {
    let (trace, setTrace) = React.useState(_ => initialTrace)

    let allNames = Array.fold_left(
      (names, f) => Belt.Set.union(names, Formula.atomicNames(f)),
      Formula.emptyNames,
      formulae,
    )

    <table className="trace-visualizer">
      <tr key="atomic-propositions">
        <th colSpan=2> {React.string("Atomic Propositions")} </th>
      </tr>
      {React.array(
        Belt.Array.mapWithIndex(Belt.Set.toArray(allNames), (i, name) => {
          let onToggle: (int, bool) => unit = (stateIndex, enabled) => {
            setTrace(Trace.setTraceState(enabled, name, stateIndex))
          }
          <tr key={string_of_int(i)}>
            <td className="formula"> <code> {React.string(String.make(1, name))} </code> </td>
            <TraceStates formula=Formula.Atomic(name) trace onToggle />
          </tr>
        }),
      )}
      <tr key="formulae"> <th colSpan=2> {React.string("Formulae")} </th> </tr>
      {React.array(
        Belt.Array.mapWithIndex(formulae, (i, formula) =>
          <tr key={string_of_int(i)}>
            <td className="formula">
              <code> {React.string(Formula.print_formula(formula))} </code>
            </td>
            <td> <TraceStates formula trace /> </td>
          </tr>
        ),
      )}
    </table>
  }
}

module App = {
  @react.component
  let make = () => {
    <div className="app">
      <header className="header">
        <h1> {React.string("Linear Temporal Logic Visualizer")} </h1>
        <TraceVisualizer initialTrace=Demo.trace formulae=Demo.formulae />
      </header>
    </div>
  }
}

switch ReactDOM.querySelector("#app-root") {
| Some(root) => ReactDOM.render(<App />, root)
| None => ()
}
