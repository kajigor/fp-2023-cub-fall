# Regex matcher

Core language:

* Bottom -- empty set
* String `s` -- set containing string `s`
* Wildcard -- set containing all single character strings
* Union `a` `b` -- union of sets `a` and `b`
* Concatenation `a` `b` -- set of strings starting with a string from `a` and ending with a string from `b`
* Repeat `r` `a` `b` -- concatenation of `r` from `a` to `b` times
* Kleene `r` -- concatenation of `r` from 0 to infinity times

From those core primitives we can build all of the tools we need:

* plus `r` -- concatenation of `r` from 1 to infinity times. It can be implemented as Concatenation `r` (Kleene `r`)
* question `r` -- matches 0 or 1 times. Can be implemented as Repeat `r` 0 1
* r{n} -- repeat `n` times. Can be implemented as Repeat `r` `n` `n`
* [abc] -- match any listed character. Can be achieved by concatenating regexes that match each of those characters.

The implementation of matcher is identical to the one from wikipedia.

# HW11

## Deadline: 05.12.2023, 23:59

### Assigned by @kajigor

1. Design a library for regular expressions.
   * There should be a set of combinators which faces user which includes those not used in the core regular expression language, such as `?`, `+`, wildcard, `{min, max}` for repetitions and so forth.
   * The core language should be small.
   * Come up with the properties which relate all combinators, including the extra user-facing combinators together.
   * Implement the matcher for regular expressions using [Brzozowski derivatives](https://en.wikipedia.org/wiki/Brzozowski_derivative).
   * Implement property-based test.
   * Supplement your code with a README which goes over the design, including the discovered properties.