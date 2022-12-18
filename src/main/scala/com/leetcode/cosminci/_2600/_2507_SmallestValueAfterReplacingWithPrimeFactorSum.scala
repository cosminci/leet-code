package com.leetcode.cosminci._2600

import scala.util.chaining.*

object _2507_SmallestValueAfterReplacingWithPrimeFactorSum:

  def smallestValue(n: Int): Int =
    (2 to n).foldLeft(n, 0) { case ((n, sum), i) =>
      Iterator
        .iterate((n, sum)) { case (n, sum) => (n / i, sum + i) }
        .dropWhile { case (n, _) => n % i == 0 }
        .next()
    }.pipe { case (_, sum) => if sum == n then sum else smallestValue(sum) }
