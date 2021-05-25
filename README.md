# Linear Temporal Logic Visualizer

✨ [quickstrom.github.io/ltl-visualizer/](https://quickstrom.github.io/ltl-visualizer/) ✨

This is an interactive visualizer for linear temporal logic (LTL), made as a
companion tool for the article [Specifying State Machines with Temporal Logic
](https://wickstrom.tech/programming/2021/05/03/specifying-state-machines-with-temporal-logic.html). Use this to play around with formulae and traces to get a better sense of how the temporal operators work.

## Usage

The rough workflow for this little application goes as follows:

1. Add formulae (see section on [Syntax](#syntax) below)
2. The atomic propositions in your formula are automatically shown above the formula
3. Toggle the truth of an atomic proposition in each respective state (click the circles!)
4. See how the truth of more complex formulae (e.g. with temporal operators) are affected when changing the atomic propositions
5. (Repeat and learn LTL for great good!)

**NOTE:** The last state in the trace is considered repeating forever. We
can't have an infinite number of checkboxes on a web page, but we want
to mess around with infinite traces nonetheless!

### Syntax

The most basic syntactic construct is the atomic proposition. It's denoted by a
single uppercase letter (A-Z), and it represents something that is true or false
in a given state. Here's an atomic proposition:

```js
A
```

This application uses (abuses, really!) the JavaScript parser in the browser to
parse formulae. Thus, we use JavaScript function application syntax to form
complex expressions, like those with operators.

Here's the temporal operator `next` applied to an atomic proposition:

```js
next(A)
```

The logical connectives can't be `||` and `&&` like in JavaScript, so they're called `or` and `and`:

```js
or(A, B)
and(C, D)
```

Those operators are variadic, meaning that you can pass one or more arguments to them:

```js
or(A, B, C, D)
```

Similarly, negation (`!` in JavaScript) is called `not`:

```js
not(A)
```

The available temporal operators are:

* `next`
* `eventually`
* `always`
* `until`

The first three take one parameter, e.g. `eventually(X)`. The `until` operator
takes two parameters, like `until(X, Y)`.

## Examples

Here's a few valid formulae:

```js
A
next(B)
or(A, next(C))
always(A)
until(A, or(B, C))
eventually(always(D))
```

## Hacking

- Build: `yarn build`
- Clean: `yarn clean`
- Build & watch: `yarn start`

## License

[Mozilla Public License Version 2.0](LICENSE)
