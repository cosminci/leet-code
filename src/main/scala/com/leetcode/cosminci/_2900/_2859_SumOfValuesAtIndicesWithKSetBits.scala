package com.leetcode.cosminci._2900

object _2859_SumOfValuesAtIndicesWithKSetBits:

  def sumIndicesWithKSetBits(nums: List[Int], k: Int): Int =
    nums.indices.collect { case i if Integer.bitCount(i) == k => nums(i) }.sum
