package com.leetcode.cosminci._2700

object _2698_FindPunishmentNumOfInt:

  def punishmentNumber(n: Int): Int =
    def canPartition(num: Int, target: Int): Boolean =
      if target < 0 || num < target then false
      else if num == target then true
      else Array(10, 100, 1000).exists(div => canPartition(num / div, target - num % div))

    (1 to n).collect { case i if canPartition(i * i, i) => i * i }.sum
