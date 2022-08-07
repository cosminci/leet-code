package io.github.cosminci.leetcode._2400

object _2367_NumArithmeticTriplets:

  def arithmeticTriplets(nums: Array[Int], diff: Int): Int =
    nums
      .foldLeft(Set.empty[Int], 0) { case ((seen, total), n) =>
        val cnt = if seen.contains(n - diff) && seen.contains(n - 2 * diff) then 1 else 0
        (seen + n, total + cnt)
      }
      ._2
