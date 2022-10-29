package com.leetcode.cosminci._1400

object _1338_ReduceArraySizeToHalf:

  def minSetSize(arr: Array[Int]): Int =
    arr
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .values
      .toArray
      .sorted
      .reverse
      .scanLeft(0)(_ + _)
      .indexWhere(_ >= arr.length / 2)
