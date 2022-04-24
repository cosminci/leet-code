package io.github.cosminci.leetcode._2300

object _2248_IntersectionOfMultipleArrays:

  def intersection(nums: Array[Array[Int]]): List[Int] =
    nums.reduce(_ intersect _).sorted.toList
