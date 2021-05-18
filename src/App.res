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
  let make = (~title, ~trace) => {
    let lastIndex = List.length(trace) - 1
    let stateHeaders = Array.mapi((i, _) => {
      let label = if i == lastIndex {
        React.string(string_of_int(i) ++ `..∞`)
      } else {
        React.int(i)
      }
      <th> {React.string("S")} <sub> {label} </sub> </th>
    }, Belt.List.toArray(trace))

    <tr> <th> {React.string(title)} </th> {React.array(stateHeaders)} <th /> </tr>
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

    let headerFills = React.array(Array.make(List.length(trace) + 1, <th />))

    <table className="trace-visualizer">
      <TraceHeader trace title="Atomic Proposition" />
      {React.array(
        Belt.Array.mapWithIndex(Belt.Set.toArray(allNames), (i, name) => {
          let onToggle: (int, bool) => unit = (stateIndex, enabled) => {
            setTrace(Trace.setTraceState(enabled, name, stateIndex))
          }
          <tr key={string_of_int(i)}>
            <td className="formula"> <code> {React.string(String.make(1, name))} </code> </td>
            <TraceStates formula=Formula.Atomic(name) trace onToggle />
            <td className="actions" />
          </tr>
        }),
      )}
      <TraceHeader trace title="Formula" />
      {React.array(
        Belt.Array.mapWithIndex(formulae, (i, formula) =>
          <tr key={string_of_int(i)}>
            <td className="formula">
              <code> {React.string(Formula.print_formula(formula))} </code>
            </td>
            <TraceStates formula trace />
            <td className="actions"> <button> {React.string("Remove")} </button> </td>
          </tr>
        ),
      )}
      <tr key="formulae">
        <td>
          <form> <input className="new-formula" placeholder="Enter a new formula..." /> </form>
        </td>
      </tr>
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
