package com.leetcode.cosminci._2500

object _2500_DeleteGreatestValueInEachRow:

  def deleteGreatestValue(grid: Array[Array[Int]]): Int =
    grid
      .map(_.sorted)
      .transpose
      .map(_.max)
      .sum
