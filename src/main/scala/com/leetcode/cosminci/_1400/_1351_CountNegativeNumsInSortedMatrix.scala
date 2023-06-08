package com.leetcode.cosminci._1400

import scala.util.chaining._

object _1351_CountNegativeNumsInSortedMatrix:

  def countNegatives(grid: Array[Array[Int]]): Int =
    Iterator
      .iterate((0, grid.head.length - 1, 0)) { case (r, c, cnt) =>
        if grid(r)(c) < 0 then (r, c - 1, cnt + grid.length - r) else (r + 1, c, cnt)
      }
      .dropWhile { case (r, c, _) => r < grid.length && c >= 0 }.next()
      .pipe { case (_, _, cnt) => cnt }
