package com.leetcode.cosminci._2800

object _2784_CheckIfArrayIsGood:

  def isGood(nums: Array[Int]): Boolean =
    nums.sorted.toSeq == (1 until nums.length) :+ nums.length - 1
