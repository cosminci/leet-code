package com.leetcode.cosminci._3000

import scala.util.chaining.*

object _2934_MinOpsToMaximizeLastElemsInArrays:

  def minOperations(nums1: Array[Int], nums2: Array[Int]): Int =
    nums1.zip(nums2)
      .foldLeft(0, 0) { case ((dp1, dp2), (a, b)) =>
        if a.max(b) > nums1.last.max(nums2.last) then return -1
        else if a.min(b) < nums1.last.min(nums2.last) then return -1
        else (
          if a > nums1.last || b > nums2.last then dp1 + 1 else dp1,
          if a > nums2.last || b > nums1.last then dp2 + 1 else dp2
        )
      }.pipe { case (dp1, dp2) => dp1.min(dp2) }
