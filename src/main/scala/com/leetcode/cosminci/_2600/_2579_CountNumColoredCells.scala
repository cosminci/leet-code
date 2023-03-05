package com.leetcode.cosminci._2600

object _2579_CountNumColoredCells:

  def coloredCells(n: Int): Long =
    n.toLong * (n - 1) * 2 + 1
