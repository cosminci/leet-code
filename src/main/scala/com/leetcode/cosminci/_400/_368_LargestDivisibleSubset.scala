package com.leetcode.cosminci._400

object _368_LargestDivisibleSubset:
  def main(args: Array[String]): Unit =
    println(largestDivisibleSubset(Array(1, 2, 3)))
    println(largestDivisibleSubset(Array(1, 2, 4, 8)))

  def largestDivisibleSubset(nums: Array[Int]): List[Int] =
    nums.sorted
      .foldLeft(Map.empty[Int, Set[Int]]) { case (divisorSubsets, n) =>
        val nDivisors = divisorSubsets
          .collect { case (d, divisors) if n % d == 0 => divisors + n }
          .maxByOption(_.size)
          .getOrElse(Set(n))

        divisorSubsets.updated(n, nDivisors)
      }.values.maxBy(_.size).toList
