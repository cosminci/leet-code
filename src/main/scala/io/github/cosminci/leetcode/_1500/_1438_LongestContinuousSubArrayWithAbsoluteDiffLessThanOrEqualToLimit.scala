package io.github.cosminci.leetcode._1500

import java.util.{Comparator, TreeSet}

object _1438_LongestContinuousSubArrayWithAbsoluteDiffLessThanOrEqualToLimit:
  def main(args: Array[String]): Unit =
    println(longestSubarray(Array(8, 2, 4, 7), 4))
    println(longestSubarray(Array(10, 1, 2, 4, 7, 2), 5))
    println(longestSubarray(Array(4, 2, 2, 2, 4, 4, 2, 2), 0))

  private def longestSubarray(nums: Array[Int], limit: Int): Int =
    val comparator: Comparator[Int] = (i, j) =>
      val valueComparison = nums(i).compare(nums(j))
      if valueComparison != 0 then valueComparison else i.compare(j)
    val window = new TreeSet[Int](comparator)
    window.add(0)

    var max    = 1
    var (l, r) = (0, 1)

    while r < nums.length do
      while r < nums.length && nums(window.last()) - nums(window.first()) <= limit do
        window.add(r)
        r += 1
        if nums(window.last()) - nums(window.first()) <= limit then max = math.max(max, window.size())
      while nums(window.last()) - nums(window.first()) > limit do
        window.remove(l)
        l += 1

    max
