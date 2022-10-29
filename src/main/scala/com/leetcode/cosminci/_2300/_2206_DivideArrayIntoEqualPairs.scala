package com.leetcode.cosminci._2300

object _2206_DivideArrayIntoEqualPairs:
  def divideArray(nums: Array[Int]): Boolean =
    nums
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .forall { case (n, count) => count % 2 == 0 }
