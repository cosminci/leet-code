package com.leetcode.cosminci._2700

object _2605_FormSmallestNumFromTwoDigitArrays:

  def minNumber(nums1: Array[Int], nums2: Array[Int]): Int =
    nums1.intersect(nums2).minOption match
      case Some(min) => min
      case None =>
        val min1 = nums1.min
        val min2 = nums2.min
        min1.min(min2) * 10 + min1.max(min2)
