package com.leetcode.cosminci._2500

import com.leetcode.cosminci.utils.lcm

object _2470_NumSubarraysWithLcmEqualToK:

  def subarrayLCM(nums: Array[Int], k: Int): Int =
    def count(j: Int, currLcm: Int): Int =
      if j == nums.length then 0
      else
        val newLcm = lcm(currLcm, nums(j))
        if newLcm > k then 0
        else if newLcm == k then 1 + count(j + 1, newLcm)
        else count(j + 1, newLcm)

    nums.indices.foldLeft(0) { (cnt, i) => cnt + count(j = i, currLcm = 1) }
