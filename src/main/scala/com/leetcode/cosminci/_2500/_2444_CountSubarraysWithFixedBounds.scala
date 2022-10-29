package com.leetcode.cosminci._2500

object _2444_CountSubarraysWithFixedBounds:

  def countSubarrays(nums: Array[Int], minK: Int, maxK: Int): Long =
    nums.zipWithIndex
      .foldLeft(0L, 0, -1, -1) { case ((count, left, lastMinK, lastMaxK), (num, right)) =>
        if num < minK || num > maxK then
          (count, right + 1, lastMinK, lastMaxK)
        else
          val newLastMinK = if num == minK then right else lastMinK
          val newLastMaxK = if num == maxK then right else lastMaxK
          val newCount =
            if newLastMinK < left || newLastMaxK < left then 0
            else newLastMinK.min(newLastMaxK) - left + 1

          (count + newCount, left, newLastMinK, newLastMaxK)
      }._1
