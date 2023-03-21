package com.leetcode.cosminci._2600

import math.floorMod as mod

object _2598_SmallestMissingNonNegativeIntAfterOps:

  def findSmallestInteger(nums: Array[Int], value: Int): Int =
    val counter = nums.map(n => mod(n, value)).groupMapReduce(identity)(_ => 1)(_ + _)
    nums.indices.foldLeft(counter) { (counter, i) =>
      if counter.get(mod(i, value)).forall(_ == 0) then return i
      counter.updated(mod(i, value), counter(mod(i, value)) - 1)
    }
    nums.length
