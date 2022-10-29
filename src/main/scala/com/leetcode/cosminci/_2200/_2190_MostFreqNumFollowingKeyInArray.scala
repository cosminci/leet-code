package com.leetcode.cosminci._2200

object _2190_MostFreqNumFollowingKeyInArray:
  def mostFrequent(nums: Array[Int], key: Int): Int =
    (1 until nums.length)
      .foldLeft(Map.empty[Int, Int].withDefaultValue(0)) { case (counts, i) =>
        if nums(i - 1) != key then counts
        else counts.updated(nums(i), counts(nums(i)) + 1)
      }
      .maxBy { case (n, count) => count }
      ._1
