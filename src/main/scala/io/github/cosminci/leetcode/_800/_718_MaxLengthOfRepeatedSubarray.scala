package io.github.cosminci.leetcode._800

object _718_MaxLengthOfRepeatedSubarray:

  def findLength(nums1: Array[Int], nums2: Array[Int]): Int =
    val dp = Array.ofDim[Int](nums1.length + 1, nums2.length + 1)

    for
      i <- nums1.indices
      j <- nums2.indices
    do dp(i + 1)(j + 1) = if nums1(i) == nums2(j) then 1 + dp(i)(j) else 0

    dp.map(_.max).max
