package com.leetcode.cosminci._200

object _164_MaximumGap:

  def maximumGap(nums: Array[Int]): Int =
    val (min, max, n) = (nums.min, nums.max, nums.length)
    if n <= 2 || min == max then return max - min

    val buckets = nums.foldLeft(Map.empty[Int, Array[Int]]) { (buckets, num) =>
      val idx = if num == max then n - 2 else ((num - min).toLong * (n - 1) / (max - min)).toInt
      buckets.updated(idx, buckets.getOrElse(idx, Array.empty[Int]) :+ num)
    }

    (0 until n - 1)
      .collect { case i if buckets.contains(i) => (buckets(i).min, buckets(i).max) }
      .sliding(2)
      .map { case Seq((_, max0), (min1, _)) => min1 - max0 }
      .max
