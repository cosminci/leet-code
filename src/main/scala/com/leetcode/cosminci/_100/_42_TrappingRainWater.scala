package com.leetcode.cosminci._100

import scala.collection.mutable

object _42_TrappingRainWater:

  def trap(heights: Array[Int]): Int =
    @annotation.tailrec
    def dfs(l: Int, r: Int, maxL: Int, maxR: Int, max: Int): Int =
      if l >= r then max
      else if maxL <= maxR then dfs(l + 1, r, maxL.max(heights(l + 1)), maxR, max + 0.max(maxL - heights(l + 1)))
      else dfs(l, r - 1, maxL, maxR.max(heights(r - 1)), max + 0.max(maxR - heights(r - 1)))

    if heights.isEmpty then 0
    else dfs(l = 0, r = heights.indices.last, heights.head, heights.last, max = 0)
