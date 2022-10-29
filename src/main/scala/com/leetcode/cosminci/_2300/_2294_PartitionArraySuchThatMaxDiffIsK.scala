package com.leetcode.cosminci._2300

object _2294_PartitionArraySuchThatMaxDiffIsK:

  def partitionArray(nums: Array[Int], k: Int): Int =
    nums.sorted.tail
      .foldLeft(1, nums.min, nums.min) { case ((res, prevMin, prevMax), n) =>
        val (min, max) = (prevMin.min(n), prevMax.max(n))
        if max - min > k then (res + 1, n, n) else (res, min, max)
      }._1
