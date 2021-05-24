module CharCmp = Belt.Id.MakeComparable({
  type t = char
  let cmp = Pervasives.compare
})

type state = Belt.Set.t<char, CharCmp.identity>

let stateOf: array<char> => state = states => Belt.Set.fromArray(states, ~id=module(CharCmp))

type trace = list<state>

let print_trace: trace => string = trace => {
  let print_state: state => string = state =>
    Array.fold_left((a, b) => a ++ String.make(1, b), "", state->Belt.Set.toArray)

  "[" ++ Js.Array2.joinWith(Belt.List.toArray(List.map(print_state, trace)), ", ") ++ "]"
}

let setTraceState: (bool, char, int, trace) => trace = (enabled, name, i, trace) => {
  let modifyState = s =>
    if enabled {
      Belt.Set.add(s, name)
    } else {
      Belt.Set.remove(s, name)
    }
  List.mapi((i', s) =>
    if i == i' {
      modifyState(s)
    } else {
      s
    }
  , trace)
}
