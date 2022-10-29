package com.leetcode.cosminci._800

object _795_NumberOfSubarraysWithBoundedMax:
  def main(args: Array[String]): Unit =
    println(numSubarrayBoundedMax(Array(0, 3, 1, 4, 5, 2, 1, 5, 10, 6), 3, 6))

  def numSubarrayBoundedMax(nums: Array[Int], left: Int, right: Int): Int =
    nums.indices.foldLeft(0, -1, -1) {
      case ((count, l0, r0), idx) =>
        val l = if nums(idx) > right then idx else l0
        val r = if nums(idx) >= left then idx else r0
        (count + r - l, l, r)
      }._1
