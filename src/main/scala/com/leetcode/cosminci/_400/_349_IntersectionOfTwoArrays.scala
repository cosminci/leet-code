package com.leetcode.cosminci._400

object _349_IntersectionOfTwoArrays:

  def intersection(nums1: Array[Int], nums2: Array[Int]): Array[Int] =
    nums1.intersect(nums2).distinct
