package com.leetcode.cosminci._2600

object _2568_MinimumImpossibleOr:

  def minImpossibleOR(nums: Array[Int]): Int =
    val pows = nums.toSet
    Iterator
      .iterate(1)(_ * 2)
      .dropWhile(pow => pows.contains(pow))
      .next()
