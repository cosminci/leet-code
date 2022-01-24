package io.github.cosminci.leetcode._2200

object _2148_CountElementsWithStrictlySmallerAndGreaterElements:

  def countElements(nums: Array[Int]): Int =
    val (min, max) = (nums.min, nums.max)
    nums.count(n => n != min && n != max)
