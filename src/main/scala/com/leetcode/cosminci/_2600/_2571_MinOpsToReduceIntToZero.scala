package com.leetcode.cosminci._2600

import scala.util.chaining.*

object _2571_MinOpsToReduceIntToZero:

  def minOperations(n: Int): Int =
    Iterator
      .iterate((0, n)) { case (res, n) =>
        if (n & 3) == 3 then (res + 1, n + 1)
        else (res + (n & 1), n >> 1)
      }
      .dropWhile { case (_, n) => n > 0 }.next()
      .pipe { case (res, _) => res }
