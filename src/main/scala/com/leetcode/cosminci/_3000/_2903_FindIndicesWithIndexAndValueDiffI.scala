package com.leetcode.cosminci._3000

object _2903_FindIndicesWithIndexAndValueDiffI:

  def findIndices(nums: Array[Int], indexDifference: Int, valueDifference: Int): Array[Int] =
    val maybeResult = for
      i <- 0 until nums.length - indexDifference
      j <- i + indexDifference until nums.length
      if (nums(i) - nums(j)).abs >= valueDifference
    yield Array(i, j)

    maybeResult.headOption.getOrElse(Array(-1, -1))
