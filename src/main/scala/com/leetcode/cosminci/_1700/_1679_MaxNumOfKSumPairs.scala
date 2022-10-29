package com.leetcode.cosminci._1700

object _1679_MaxNumOfKSumPairs:

  def maxOperations(nums: Array[Int], k: Int): Int =
    nums.foldLeft(Map.empty[Int, Int].withDefaultValue(0), 0) {
      case ((counts, result), n) =>
        if counts(k - n) == 0 then (counts.updated(n, counts(n) + 1), result)
        else (counts.updated(k - n, counts(k - n) - 1), result + 1)
      }._2
