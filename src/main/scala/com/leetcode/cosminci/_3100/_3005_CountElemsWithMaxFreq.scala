package com.leetcode.cosminci._3100

object _3005_CountElemsWithMaxFreq:

  def maxFrequencyElements(nums: Array[Int]): Int =
    val freq = nums.groupMapReduce(identity)(_ => 1)(_ + _)
    val max  = freq.values.max
    freq.values.count(_ == max) * max
