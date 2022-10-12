package io.github.cosminci.leetcode._1000

object _976_LargestPerimeterTriangle:

  def largestPerimeter(nums: Array[Int]): Int =
    nums.sorted.reverse
      .sliding(3)
      .collectFirst { case Array(a, b, c) if a < b + c => a + b + c }
      .getOrElse(0)
