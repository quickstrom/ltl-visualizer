module App = {
  @react.component
  let make = () => {
    let states = Belt.Array.map(
      Belt.List.toArray(Eval.evalAll(Demo.formula, Demo.trace)),
      (result) => {
        <li>{React.string(string_of_bool(result))}</li>
      },
    )
    <div className="app">
      <header className="header">
        <h1> {React.string("Linear Temporal Logic Visualizer")} </h1>
      </header>
      <div> {React.string(Formula.print_formula(Demo.formula))} </div>
      <div> {React.string(Trace.print_trace(Demo.trace))} </div>
      {React.string("Result:")}
      <ul> {React.array(states)} </ul>
    </div>
  }
}

switch ReactDOM.querySelector("#app-root") {
| Some(root) => ReactDOM.render(<App />, root)
| None => ()
}
