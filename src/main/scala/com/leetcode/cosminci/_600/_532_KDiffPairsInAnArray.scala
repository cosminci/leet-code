package com.leetcode.cosminci._600

object _532_KDiffPairsInAnArray:
  def main(args: Array[String]): Unit =
    println(findPairs(Array(3, 1, 4, 1, 5), 2))
    println(findPairs(Array(1, 2, 3, 4, 5), 1))
    println(findPairs(Array(1, 3, 1, 5, 4), 0))
    println(findPairs(Array(1, 2, 4, 4, 3, 3, 0, 9, 2, 3), 3))

  def findPairs(nums: Array[Int], k: Int): Int =
    if k == 0 then
      nums.groupBy(identity).values.count(_.length > 1)
    else
      val distinct = nums.toSet
      distinct.count(n => distinct.contains(n + k))
