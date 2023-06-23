package com.leetcode.cosminci._1100

object _1027_LongestArithmeticSubseq:

  def longestArithSeqLength(nums: Array[Int]): Int =
    (1 until nums.length)
      .foldLeft(Map.empty[Int, Int].withDefaultValue(1)) { (acc, j) =>
        (0 until j).foldLeft(acc) { (acc, i) =>
          val diff = nums(j) - nums(i)
          acc.updated(diff, acc(diff) + 1)
        }
      }.values.max
