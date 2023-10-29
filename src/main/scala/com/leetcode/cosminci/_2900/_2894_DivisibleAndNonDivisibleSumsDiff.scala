package com.leetcode.cosminci._2900

object _2894_DivisibleAndNonDivisibleSumsDiff:

  def differenceOfSums(n: Int, m: Int): Int =
    val (nonDivisible, divisible) = (1 to n).partitionMap(i => Either.cond(i % m == 0, i, i))
    nonDivisible.sum - divisible.sum
