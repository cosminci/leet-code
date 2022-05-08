package io.github.cosminci.leetcode._2300

object _2261_KDivisibleElementsSubarrays:

  def countDistinct(nums: Array[Int], k: Int, p: Int): Int =
    val prefixCount = nums.scanLeft(0)((numDivisible, num) => numDivisible + Option.when(num % p == 0)(1).getOrElse(0))
    (1 to nums.length).map { length =>
      (0 to nums.length - length)
        .collect { case i if prefixCount(i + length) - prefixCount(i) <= k => (i, i + length) }
        .distinctBy { case (i, j) => nums.slice(i, j).toSeq }
        .length
    }.sum
