package com.leetcode.cosminci._2700

object _2640_FindScoreOfAllPrefixesOfArray:

  def findPrefixScore(nums: Array[Int]): Array[Long] =
    nums.scanLeft(0L)(_ max _).tail.zip(nums)
      .map { case (max, num) => max + num }
      .scanLeft(0L)(_ + _).tail
