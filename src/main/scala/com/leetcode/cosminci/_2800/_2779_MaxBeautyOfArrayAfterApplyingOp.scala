package com.leetcode.cosminci._2800

import scala.util.chaining.*

object _2779_MaxBeautyOfArrayAfterApplyingOp:

  def maximumBeauty(nums: Array[Int], k: Int): Int =
    nums.sorted.pipe { nums =>
      nums.indices
        .foldLeft(0, 0) { case ((max, i), j) =>
          if nums(j) - nums(i) > 2 * k then (max, i + 1)
          else (max.max(j - i + 1), i)
        }
        .pipe { case (max, _) => max }
    }
