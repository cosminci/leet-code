package com.leetcode.cosminci._100

object _27_RemoveElement:

  def removeElement(nums: Array[Int], v: Int): Int =
    nums.indices.foldLeft(0) {
      case (k, i) if nums(i) == v => k
      case (k, i) =>
        nums(k) = nums(i)
        k + 1
    }
