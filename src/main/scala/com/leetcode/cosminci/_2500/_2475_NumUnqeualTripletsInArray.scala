package com.leetcode.cosminci._2500

object _2475_NumUnqeualTripletsInArray:

  def unequalTriplets(nums: Array[Int]): Int =
    nums
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .foldLeft(0, 0, nums.length) { case ((result, left, right), (_, freq)) =>
        (result + left * freq * (right - freq), left + freq, right - freq)
      }._1
