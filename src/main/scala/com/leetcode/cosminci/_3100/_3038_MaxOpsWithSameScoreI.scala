package com.leetcode.cosminci._3100

object _3038_MaxOpsWithSameScoreI:

  def maxOperations(nums: Array[Int]): Int =
    (nums :+ Int.MaxValue)
      .grouped(2).map(_.sum)
      .sliding(2).takeWhile { case Seq(a, b) => a == b }
      .size + 1
