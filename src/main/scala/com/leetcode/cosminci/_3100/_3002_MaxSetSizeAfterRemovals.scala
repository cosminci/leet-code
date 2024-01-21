package com.leetcode.cosminci._3100

object _3002_MaxSetSizeAfterRemovals:

  def maximumSetSize(nums1: Array[Int], nums2: Array[Int]): Int =
    val (set1, set2) = (nums1.toSet, nums2.toSet)
    val (s1, s2, s3) = (set1.size, set2.size, (set1 & set2).size)
    val (n1, n2)     = (nums1.length, nums2.length)
    (s1 + s2 - s3).min(s1.min(n1 / 2) + s2.min(n2 / 2))
