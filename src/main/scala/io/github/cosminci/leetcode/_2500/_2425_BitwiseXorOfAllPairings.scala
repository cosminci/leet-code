package io.github.cosminci.leetcode._2500

object _2425_BitwiseXorOfAllPairings:

  def xorAllNums(nums1: Array[Int], nums2: Array[Int]): Int =
    Option.when(nums1.length % 2 == 0)(0).getOrElse(nums2.reduce(_ ^ _)) ^
      Option.when(nums2.length % 2 == 0)(0).getOrElse(nums1.reduce(_ ^ _))
