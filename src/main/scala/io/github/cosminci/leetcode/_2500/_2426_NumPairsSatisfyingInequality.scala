package io.github.cosminci.leetcode._2500

import scala.collection.mutable

object _2426_NumPairsSatisfyingInequality:

  /*
   * Equivalent Python that passes:
   * class Solution:
   *   def numberOfPairs(self, nums1: List[int], nums2: List[int], diff: int) -> int:
   *     sl, ans = SortedList(), 0
   *     for a, b in zip(nums1, nums2):
   *       ans += sl.bisect_right(a - b + dif
   *       sl.add(a - b)
   *   return ans
   */
  def numberOfPairs(nums1: Array[Int], nums2: Array[Int], diff: Int): Long =
    val prev = mutable.ListBuffer.empty[(Int, Int)]
    nums1.zip(nums2).foldLeft(0L) { case (result, (a, b)) =>
      val i = prev.search((a - b + diff, prev.length)).insertionPoint
      val j = prev.search((a - b, prev.length)).insertionPoint
      prev.insert(j, (a - b, prev.length))
      result + i
    }
