package com.leetcode.cosminci._2600

object _2588_CountBeautifulSubarrays:

  def beautifulSubarrays(nums: Array[Int]): Long =
    nums.foldLeft(0L, 0, Map(0 -> 1L).withDefaultValue(0L)) {
      case ((res, prefixXor, prevXors), n) =>
        val xor = prefixXor ^ n
        (res + prevXors(xor), xor, prevXors.updated(xor, prevXors(xor) + 1))
    }._1
