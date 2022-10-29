package com.leetcode.cosminci._2400

object _2341_MaxNumPairsInArray:

  def numberOfPairs(nums: Array[Int]): Array[Int] =
    nums
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .foldLeft(Array(0, 0)) { case (result, (_, count)) =>
        Array(result(0) + count / 2, result(1) + count % 2)
      }
