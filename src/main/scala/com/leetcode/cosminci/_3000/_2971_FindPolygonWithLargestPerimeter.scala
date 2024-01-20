package com.leetcode.cosminci._3000

object _2971_FindPolygonWithLargestPerimeter:

  def largestPerimeter(nums: Array[Int]): Long =
    nums.sortInPlace()
    val pSum = nums.scanLeft(0L)(_ + _)
    (nums.indices.last to 2 by -1)
      .collectFirst { case i if nums(i) < pSum(i) => pSum(i) + nums(i) }
      .getOrElse(-1L)
