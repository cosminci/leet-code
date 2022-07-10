package io.github.cosminci.leetcode._2400

import scala.collection.immutable.TreeMap

object _2333_MinSumOfSquaredDiff:

  def minSumSquareDiff(nums1: Array[Int], nums2: Array[Int], k1: Int, k2: Int): Long =
    val diffs = TreeMap.from(
      nums1.zip(nums2)
        .map { case (n1, n2) => (n1 - n2).abs }
        .groupMapReduce(identity)(_ => 1)(_ + _)
    )
    Iterator
      .iterate((diffs, k1 + k2)) { case (diffs, k) =>
        val (maxDiff, count) = diffs.last
        val diffs1 = if count.min(k) == count then diffs.removed(maxDiff) else diffs.updated(maxDiff, count - k)
        val diffs2 = diffs1.updated(maxDiff - 1, diffs.getOrElse(maxDiff - 1, 0) + count.min(k))
        (diffs2, k - count.min(k))
      }
      .dropWhile { case (diffs, k) => diffs.lastOption.exists(_._1 > 0) && k > 0 }
      .next()._1
      .map { case (diff, count) => count.toLong * diff * diff }
      .sum
