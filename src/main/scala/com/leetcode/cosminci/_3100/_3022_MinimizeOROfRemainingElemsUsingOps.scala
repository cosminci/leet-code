package com.leetcode.cosminci._3100

object _3022_MinimizeOROfRemainingElemsUsingOps:

  def minOrAfterOperations(nums: Array[Int], k: Int): Int =
    (31 to 0 by -1).foldLeft(0) { (res, bit) =>
      val available = res | (1 << bit) - 1
      val (_, cnt) = nums.foldLeft((1 << 31) - 1, 0) { case ((mask, cnt), n) =>
        if (mask & n | available) == available then ((1 << 31) - 1, cnt + 1)
        else (mask & n, cnt)
      }
      if nums.length - cnt > k then res | (1 << bit) else res
    }
