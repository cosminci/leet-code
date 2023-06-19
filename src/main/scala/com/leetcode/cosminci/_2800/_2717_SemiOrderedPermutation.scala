package com.leetcode.cosminci._2800

object _2717_SemiOrderedPermutation:

  def semiOrderedPermutation(nums: Array[Int]): Int =
    val (i, j) = (nums.indexOf(nums.min), nums.indexOf(nums.max))
    i + nums.length - j - { if i < j then 1 else 2 }
