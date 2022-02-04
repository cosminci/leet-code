package io.github.cosminci.leetcode._500

import scala.collection.mutable

object _454_FourSumII:
  def main(args: Array[String]): Unit =
    println(fourSumCount(Array(1, 2), Array(-2, -1), Array(-1, 2), Array(0, 2)))

  def fourSumCount(nums1: Array[Int], nums2: Array[Int], nums3: Array[Int], nums4: Array[Int]): Int =
    val nums12Sums = for { n1 <- nums1; n2 <- nums2 } yield n1 + n2
    val nums12SumCounts = nums12Sums.groupMapReduce(identity)(_ => 1)(_ + _)
    val nums34Sums = for { n3 <- nums3; n4 <- nums4 } yield n3 + n4
    nums34Sums.map(s => nums12SumCounts.getOrElse(-s, 0)).sum
