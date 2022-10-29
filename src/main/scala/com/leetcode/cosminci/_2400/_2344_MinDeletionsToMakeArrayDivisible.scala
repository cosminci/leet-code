package com.leetcode.cosminci._2400

import com.leetcode.cosminci.utils

object _2344_MinDeletionsToMakeArrayDivisible:

  def minOperations(nums: Array[Int], numsDivide: Array[Int]): Int =
    val gcd = numsDivide.reduce(utils.gcd)
    nums.sorted.indexWhere { n =>
      if n > gcd then return -1
      gcd % n == 0
    }
