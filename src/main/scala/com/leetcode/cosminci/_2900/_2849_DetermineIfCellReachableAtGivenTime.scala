package com.leetcode.cosminci._2900

object _2849_DetermineIfCellReachableAtGivenTime:

  def isReachableAtTime(sx: Int, sy: Int, fx: Int, fy: Int, t: Int): Boolean =
    val (dx, dy) = ((fx - sx).abs, (fy - sy).abs)
    (dx != 0 || dy != 0 || t != 1) &&
    (dx.min(dy) + (dx - dy).abs) <= t
