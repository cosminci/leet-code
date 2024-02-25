package com.leetcode.cosminci._3100

object _3028_AntOnTheBoundary:

  def returnToBoundaryCount(nums: Array[Int]): Int =
    nums.scanLeft(0)(_ + _).count(_ == 0) - 1
