package io.github.cosminci.leetcode._2400

object _2364_CountBadPairs:

  def countBadPairs(nums: Array[Int]): Long =
    nums.length.toLong * (nums.length + 1) / 2 -
      nums.zipWithIndex
        .groupMapReduce { case (n, i) => n - i }(_ => 1L)(_ + _)
        .values
        .map(cnt => cnt * (cnt + 1) / 2)
        .sum
