package com.leetcode.cosminci._1700

object _1695_MaxErasureValue:

  def maximumUniqueSubarray(nums: Array[Int]): Int =
    nums.zipWithIndex
      .foldLeft(0, 0, 0, Map.empty[Int, Int]) { case ((max, curr, start, indices), (num, j)) =>
        indices.get(num) match
          case None =>
            (max.max(curr + num), curr + num, start, indices.updated(num, j))
          case Some(i) =>
            val toSkip = nums.slice(start, i + 1)
            (max, curr - toSkip.sum + num, i + 1, indices.removedAll(toSkip).updated(num, j))
      }
      ._1
