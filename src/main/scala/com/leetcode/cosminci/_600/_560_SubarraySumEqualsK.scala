package com.leetcode.cosminci._600

object _560_SubarraySumEqualsK:
  def main(args: Array[String]): Unit =
    println(subarraySum(Array(1, 2, 3), 3))

  def subarraySum(nums: Array[Int], k: Int): Int =
    nums
      .foldLeft((Map(0 -> 1), 0, 0)) { case ((sumCounts, count, prevSum), n) =>
        val newSum       = prevSum + n
        val newCount     = count + sumCounts.getOrElse(newSum - k, 0)
        val newSumCounts = sumCounts + (newSum -> (sumCounts.getOrElse(newSum, 0) + 1))
        (newSumCounts, newCount, newSum)
      }
      ._2
