package com.leetcode.cosminci._2700

import scala.util.chaining.*

object _2644_FindMatrixDivisibilityScore:

  def maxDivScore(nums: Array[Int], divisors: Array[Int]): Int =
    divisors
      .foldLeft(0, -1) { case ((globalMax, currMax), div) =>
        val count = nums.count(_ % div == 0)
        if count < currMax then (globalMax, currMax)
        else if count > currMax then (div, count)
        else (div.min(globalMax), count)
      }
      .pipe { case (res, _) => res }
