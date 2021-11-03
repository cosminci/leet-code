package io.github.cosminci.leetcode._2000

object _1991_FindTheMiddleIndexInArray:
  def main(args: Array[String]): Unit =
    println(findMiddleIndex(Array(1)))
    println(findMiddleIndex(Array(2, 3, -1, 8, 4)))
    println(findMiddleIndex(Array(1, -1, 4)))
    println(findMiddleIndex(Array(2, 5)))

  def findMiddleIndex(nums: Array[Int]): Int =
    val prefixSums = nums.scanLeft(0)(_ + _)
    nums.indices.indexWhere(i => prefixSums(i) == prefixSums.last - prefixSums(i) - nums(i))
