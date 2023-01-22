package com.leetcode.cosminci._2600

object _2540_MinCommonValue:

  def getCommon(nums1: Array[Int], nums2: Array[Int]): Int =
    nums1.intersect(nums2).minOption.getOrElse(-1)
