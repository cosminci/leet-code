package io.github.cosminci.leetcode._1900

import scala.collection.mutable

object _1856_MaxSubarrayMinProduct:
  def main(args: Array[String]): Unit =
    println(maxSumMinProduct(Array(1, 2, 3, 2)))
    println(maxSumMinProduct(Array(2, 3, 3, 1, 2)))
    println(maxSumMinProduct(Array(3, 1, 5, 6, 4, 2)))

  def maxSumMinProduct(nums: Array[Int]): Int =
    var max        = BigInt(0)
    val prefixSums = nums.scanLeft(BigInt(0))(_ + _)

    val monotonicallyIncreasing = mutable.Stack.empty[(BigInt, Int)]
    nums.appended(0).zipWithIndex.foreach { case (n, currIdx) =>
      var idx = currIdx
      while monotonicallyIncreasing.nonEmpty && monotonicallyIncreasing.head._1 > n do
        val (prev, prevIdx) = monotonicallyIncreasing.pop()
        max = max.max(prev * (prefixSums(currIdx) - prefixSums(prevIdx)))
        idx = prevIdx
      monotonicallyIncreasing.push((n, idx))
    }

    (max % (math.pow(10, 9) + 7).toInt).toInt
