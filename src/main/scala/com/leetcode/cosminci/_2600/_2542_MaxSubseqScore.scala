package com.leetcode.cosminci._2600

import scala.collection.immutable.TreeSet
import scala.util.chaining.*

object _2542_MaxSubseqScore:

  def maxScore(nums1: Array[Int], nums2: Array[Int], k: Int): Long =
    nums1.indices
      .sortBy(i => -nums2(i))
      .foldLeft(0L, 0L, TreeSet.empty[(Int, Int)]) { case ((result, sum, best), i) =>
        (best + (nums1(i) -> i), sum + nums1(i))
          .pipe { case (b, s) => if b.size <= k then (b, s) else (b.tail, s - b.head.pipe { case (n, _) => n }) }
          .pipe { case (b, s) => if b.size != k then (result, s, b) else (result.max(s * nums2(i)), s, b) }
      }
      .pipe { case (result, _, _) => result }
