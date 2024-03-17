package com.leetcode.cosminci._3100

object _3046_SplitTheArray:

  def isPossibleToSplit(nums: Array[Int]): Boolean =
    nums.groupMapReduce(identity)(_ => 1)(_ + _).values.max <= 2
