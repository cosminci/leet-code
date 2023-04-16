package com.leetcode.cosminci._2700

object _2639_FindWidthOfGridColumns:

  def findColumnWidth(grid: Array[Array[Int]]): Array[Int] =
    grid.transpose.map(col => col.map(_.toString.length).max)
