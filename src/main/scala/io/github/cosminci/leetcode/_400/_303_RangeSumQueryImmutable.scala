package io.github.cosminci.leetcode._400

object _303_RangeSumQueryImmutable {
  class NumArray(_nums: Array[Int]) {
    private val prefixSums = _nums.scanLeft(0)(_ + _)
    
    def sumRange(left: Int, right: Int): Int = 
      prefixSums(right + 1) - prefixSums(left)
  }
}
