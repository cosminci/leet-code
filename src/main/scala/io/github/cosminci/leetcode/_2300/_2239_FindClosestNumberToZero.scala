package io.github.cosminci.leetcode._2300

object _2239_FindClosestNumberToZero {

  def findClosestNumber(nums: Array[Int]): Int = nums.sortBy(n => -n).minBy(_.abs)

}
