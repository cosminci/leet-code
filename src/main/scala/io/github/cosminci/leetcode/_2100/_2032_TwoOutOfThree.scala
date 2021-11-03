package io.github.cosminci.leetcode._2100

object _2032_TwoOutOfThree:

  def twoOutOfThree(nums1: Array[Int], nums2: Array[Int], nums3: Array[Int]): List[Int] =
    val present = Array.ofDim[Int](101, 3)
    nums1.foreach(n => present(n)(0) = 1)
    nums2.foreach(n => present(n)(1) = 1)
    nums3.foreach(n => present(n)(2) = 1)
    present.indices.filter(present(_).sum >= 2).toList
