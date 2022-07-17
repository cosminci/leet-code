package io.github.cosminci.leetcode._2400

object _2343_QueryKthSmallestTrimmedNum:

  def smallestTrimmedNumbers(nums: Array[String], queries: Array[Array[Int]]): Array[Int] =
    queries.map { case Array(k, trim) =>
      nums
        .map(_.takeRight(trim))
        .zipWithIndex
        .sorted
        .apply(k - 1)
        ._2
    }
