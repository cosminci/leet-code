package com.leetcode.cosminci._2900

object _2856_MinArrayLenAfterPairRemovals:

  def minLengthAfterRemovals(nums: List[Int]): Int =
    val counter = nums.groupMapReduce(identity)(_ => 1)(_ + _)
    val maxFreq = counter.values.max
    if maxFreq <= nums.length / 2 then if nums.length % 2 == 0 then 0 else 1
    else maxFreq - (nums.length - maxFreq)
