package com.leetcode.cosminci._3000

object _2917_FindKorOfAnArray:

  def findKOr(nums: Array[Int], k: Int): Int =
    (0 to 30).foldLeft(0) { (res, bit) =>
      if nums.count(n => (n >> bit & 1) > 0) >= k then res | (1 << bit) else res
    }
