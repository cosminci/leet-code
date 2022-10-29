package com.leetcode.cosminci._1300

object _1260_Shift2DGrid:

  def shiftGrid(grid: Array[Array[Int]], k: Int): List[List[Int]] =
    val (nums, cols) = (grid.flatten, grid(0).length)
    val (tail, head) = nums.splitAt(nums.length - k % nums.length)
    (head ++ tail).grouped(cols).map(_.toList).toList
