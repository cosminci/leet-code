package com.leetcode.cosminci._2300

object _2215_FindTheDiffOfTwoArrays {

  def findDifference(nums1: Array[Int], nums2: Array[Int]): List[List[Int]] = List(
    nums1.distinct.diff(nums2).toList,
    nums2.distinct.diff(nums1).toList
  )
}
