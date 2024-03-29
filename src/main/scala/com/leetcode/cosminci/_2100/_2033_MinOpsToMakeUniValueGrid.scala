package com.leetcode.cosminci._2100

object _2033_MinOpsToMakeUniValueGrid {
  def minOperations(grid: Array[Array[Int]], x: Int): Int = {
    val sortedValues = grid.flatten.sorted
    val median = sortedValues(sortedValues.length / 2)
    sortedValues.foldLeft(0) { (count, value) =>
      if ((value - median) % x != 0) return -1
      count + math.abs(value - median) / x
    }
  }
}
