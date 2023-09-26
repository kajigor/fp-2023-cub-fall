# HW01 

## Deadline: 26.09.2023, 23:59

### Assigned by @danyaberezun

1. Select a functional language you want (or several) =)
2. Implement RB-trees in both functional and imperative-like fashion.
   1. implement function `fromOrdList :: List -> Tree` which converts the sorted list with no repeats to a RB-tree running in `O(n)`
   2. Reduce the number of unnecessary checks in function `balance`
      1. split `balance` onto two functions `lbalance` and `rbalance` (checks that left and right child ensures invariants); use them in `ins` function instead of `balance`
      2. Rewrite `ins` in a way it never checks the colors of his grandchildren
   3. Implement benchmarks that compare the actual execution times of these implementations
3. Implement all 4 queues; Implement benchmarks that compare the actual execution times of these implementations

[Slides from the lecture](lec1.pdf)

[The recording of the lecture](https://www.youtube.com/watch?v=9TTcy4mkvn0&list=PLQsQ42jQ8PJEF8KmvQfSghy6oGXpXr5Am&index=1)
