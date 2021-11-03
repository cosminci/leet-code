package io.github.cosminci.leetcode._100

object _11_ContainerWithMostWater:

  def main(args: Array[String]): Unit =
    println(maxArea(Array(1, 8, 6, 100, 90, 4, 8, 3, 7)))

  def maxArea(heights: Array[Int]): Int =
    var (l, r) = (0, heights.length - 1)
    var max    = area(heights, l, r)
    while l < r do
      if heights(l) < heights(r) then l += 1
      else r -= 1
      val newArea = area(heights, l, r)
      max = math.max(max, newArea)
    max

  private def area(heights: Array[Int], startIdx: Int, endIdx: Int) =
    math.min(heights(startIdx), heights(endIdx)) * (endIdx - startIdx)
