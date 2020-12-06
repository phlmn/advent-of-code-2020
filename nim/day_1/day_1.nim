import sequtils, sugar, strutils, algorithm

proc apl[T, S](val: T, fn: proc (x: T): S): S =
  fn(val)

open("day_1.txt")
  .readAll
  .split('\n')
  .map(parseInt)
  .apl(a => @[a, a, a])
  .product
  .filter(pair => pair.foldl(a+b) == 2020)
  .map(pair => pair.sorted)
  .deduplicate
  .map(pair => pair.foldl(a * b, 1))
  .echo
