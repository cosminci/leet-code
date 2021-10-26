package io.github.cosminci.leetcode._800

object _724_FindPivotIndex:
  def main(args: Array[String]): Unit =
    println(pivotIndexScan(Array(1, 7, 3, 6, 5, 6)))
    println(pivotIndexDelta(Array(1, 7, 3, 6, 5, 6)))
    println(pivotIndexScan(Array(1, 2, 3)))
    println(pivotIndexDelta(Array(1, 2, 3)))
    println(pivotIndexScan(Array(2, 1, -1)))
    println(pivotIndexDelta(Array(2, 1, -1)))

  private def pivotIndexScan(nums: Array[Int]): Int =
    val prefixSums = nums.scanLeft(0)(_ + _).tail
    val suffixSums = nums.scanRight(0)(_ + _).dropRight(1)
    prefixSums.indices.find(i => prefixSums(i) == suffixSums(i)).getOrElse(-1)

  private def pivotIndexDelta(nums: Array[Int]): Int =
    val total          = nums.sum
    var (l, prefixSum) = (0, 0)
    while l < nums.length do
      if prefixSum == total - prefixSum - nums(l) then return l
      prefixSum += nums(l)
      l += 1
    -1
