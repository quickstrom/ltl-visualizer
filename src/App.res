module App = {
  @react.component
  let make = () => {
    <div> 
    <div> 
    {React.string(Formula.print_formula(Demo.Test.formula))} 
    </div>
    <div> 
    {React.string(Trace.print_trace(Demo.Test.trace))} 
    </div>
    <div> 
    {React.string("Result:")} 
    {React.string(string_of_bool(Eval.eval(Demo.Test.formula, Demo.Test.trace)))} 
    </div>
    </div>
  }
}

switch ReactDOM.querySelector("#app-root") {
| Some(root) => ReactDOM.render(<App />, root)
| None => ()
}
