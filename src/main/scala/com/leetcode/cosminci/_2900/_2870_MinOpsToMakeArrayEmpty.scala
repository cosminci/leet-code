package com.leetcode.cosminci._2900

object _2870_MinOpsToMakeArrayEmpty:

  def minOperations(nums: Array[Int]): Int =
    nums.groupMapReduce(identity)(_ => 1)(_ + _).values.foldLeft(0) { (ops, count) =>
      if count == 1 then return -1
      ops + count / 3 + (if count % 3 > 0 then 1 else 0)
    }
