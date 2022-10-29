package com.leetcode.cosminci._2400

object _2321_MaxScoreOfSplicedArray:

  def maximumsSplicedArray(nums1: Array[Int], nums2: Array[Int]): Int = {
    def kadane(nums1: Array[Int], nums2: Array[Int]): Int =
      nums1.indices
        .scanLeft(0)((sum, i) => (sum + nums2(i) - nums1(i)).max(nums2(i) - nums1(i)))
        .max

    (nums1.sum + kadane(nums1, nums2)).max(nums2.sum + kadane(nums2, nums1))
  }
