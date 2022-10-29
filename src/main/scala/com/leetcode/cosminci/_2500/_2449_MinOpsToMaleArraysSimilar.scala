package com.leetcode.cosminci._2500

object _2449_MinOpsToMaleArraysSimilar:

  def makeSimilar(nums: Array[Int], target: Array[Int]): Long =
    nums.sortInPlaceBy(n => (n % 2, n))
    target.sortInPlaceBy(n => (n % 2, n))

    nums.zip(target).map { case (n, t) => (n - t).abs.toLong }.sum / 4
