package io.github.cosminci.leetcode._2100

object _2091_RemovingMinAndMaxFromArray:
  def minimumDeletions(nums: Array[Int]): Int =
    val (i, j, n) = (nums.indexOf(nums.max), nums.indexOf(nums.min), nums.length)
    Seq(
      (i + 1).max(j + 1),
      (n - i).max(n - j),
      i + 1 + n - j,
      j + 1 + n - i
    ).min
