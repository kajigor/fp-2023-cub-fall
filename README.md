# HW07

## Deadline: 07.11.2023, 23:59

### Assigned by @kajigor

1. Make `typeCheck` and `typeCheckEmpty` return `Either String Type`. Make sure that the app still runs.
2. Add the let expression `(let ... = ... in ...)` and arithmetic expressions over integer numbers into the language. Support their parsing and typechecking.
3. Print proof trees for `typeCheck` into a `.tex` file so that they can be nicely displayed as in the assignment from HW05.
   * Add a new option into the app so that the user can pass the output file name.
   * Make sure that the generated `.tex` file can be compiled without extra effort. It would be better if your app just runs a latex compiler to generate a `.pdf` file with the tree.
4. Implement tests.