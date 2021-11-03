package io.github.cosminci.leetcode._800

object _718_MaxLengthOfRepeatedSubarray:
  def main(args: Array[String]): Unit =
    println(findLength(Array(1, 2, 3, 2, 1), Array(3, 2, 1, 4, 7)))

  def findLength(nums1: Array[Int], nums2: Array[Int]): Int =
    val dp  = Array.ofDim[Int](nums1.length + 1, nums2.length + 1)
    var max = 0
    nums1.indices.foreach { n1Idx =>
      val n1DpIdx = n1Idx + 1
      val n1      = nums1(n1Idx)
      nums2.indices.foreach { n2Idx =>
        val n2DpIdx = n2Idx + 1
        val n2      = nums2(n2Idx)
        dp(n1DpIdx)(n2DpIdx) = if n1 != n2 then 0 else 1 + dp(n1DpIdx - 1)(n2DpIdx - 1)
        if dp(n1DpIdx)(n2DpIdx) > max then max = dp(n1DpIdx)(n2DpIdx)
      }
    }
    max
