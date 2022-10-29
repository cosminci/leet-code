package com.leetcode.cosminci._100

object _11_ContainerWithMostWater:

  def main(args: Array[String]): Unit =
    println(maxArea(Array(1, 8, 6, 100, 90, 4, 8, 3, 7)))

  def maxArea(heights: Array[Int]): Int =
    def area(l: Int, r: Int): Int = (heights(l) min heights(r)) * (r - l)

    @annotation.tailrec
    def dfs(l: Int, r: Int, prevMax: Int): Int =
      if l >= r then prevMax
      else if heights(l) < heights(r) then dfs(l + 1, r, prevMax.max(area(l + 1, r)))
      else dfs(l, r - 1, prevMax.max(area(l, r - 1)))

    dfs(l = 0, r = heights.length - 1, area(0, heights.length - 1))
