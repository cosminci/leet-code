package io.github.cosminci.leetcode._300

object _217_ContainsDuplicate:
  def containsDuplicate(nums: Array[Int]): Boolean =
    nums.distinct.length != nums.length
