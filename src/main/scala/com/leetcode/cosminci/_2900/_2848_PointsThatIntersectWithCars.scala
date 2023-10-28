package com.leetcode.cosminci._2900

object _2848_PointsThatIntersectWithCars:

  def numberOfPoints(nums: List[List[Int]]): Int =
    nums
      .collect { case l :: r :: Nil => (l to r).toSet }
      .reduce(_ union _)
      .size
