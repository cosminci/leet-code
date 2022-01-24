package io.github.cosminci.leetcode._2200

object _2150_FindAllLonelyNumbersInTheArray:

  def findLonely(nums: Array[Int]): List[Int] =
    val counts = nums.groupMapReduce(identity)(_ => 1)(_ + _)
    counts.keys
      .filter(k => counts(k) == 1 && !counts.contains(k - 1) && !counts.contains(k + 1))
      .toList
