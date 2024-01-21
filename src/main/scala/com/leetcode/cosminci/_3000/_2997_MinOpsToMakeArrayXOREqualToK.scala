package com.leetcode.cosminci._3000

object _2997_MinOpsToMakeArrayXOREqualToK:

  def minOperations(nums: Array[Int], k: Int): Int =
    val binaryNums = nums.map(_.toBinaryString.reverse.padTo(32, '0').reverse)
    val target     = k.toBinaryString.reverse.padTo(32, '0').reverse
    (0 until 32).count(i => binaryNums.map(_.charAt(i)).count(_ == '1') % 2 != target.charAt(i) - '0')
