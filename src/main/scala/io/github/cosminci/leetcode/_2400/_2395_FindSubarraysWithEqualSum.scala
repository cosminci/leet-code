package io.github.cosminci.leetcode._2400

object _2395_FindSubarraysWithEqualSum:

  def findSubarrays(nums: Array[Int]): Boolean =
    nums
      .map(_.toLong)
      .sliding(2)
      .map(_.sum)
      .toSeq
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .exists { case (_, cnt) => cnt > 1 }
