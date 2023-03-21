package com.leetcode.cosminci._2600

object _2592_MaxGreatnessOfArray:

  def maximizeGreatness(nums: Array[Int]): Int =
    nums.length - nums.groupMapReduce(identity)(_ => 1)(_ + _).values.max
