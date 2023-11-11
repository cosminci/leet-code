package com.leetcode.cosminci._3000

object _2918_MinEqualSumOfTwoArraysAfterReplacingZeros:

  def minSum(nums1: Array[Int], nums2: Array[Int]): Long =
    val (z1, z2) = (nums1.count(_ == 0), nums2.count(_ == 0))
    val (s1, s2) = (nums1.foldLeft(0L)(_ + _) + z1, nums2.foldLeft(0L)(_ + _) + z2)
    if (s1 < s2 && z1 == 0) || (s2 < s1 && z2 == 0) then -1
    else s1.max(s2)
