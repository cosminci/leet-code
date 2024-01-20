package com.leetcode.cosminci._3000

object _2974_MinNumGame:

  def numberGame(nums: Array[Int]): Array[Int] =
    nums.sorted.grouped(2).flatMap(_.reverse).toArray
