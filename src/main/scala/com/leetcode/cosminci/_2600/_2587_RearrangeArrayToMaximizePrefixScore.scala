package com.leetcode.cosminci._2600

import scala.util.chaining.*

object _2587_RearrangeArrayToMaximizePrefixScore:

  def maxScore(nums: Array[Int]): Int =
    nums
      .sortBy(v => -v)
      .foldLeft(0, 0L) { case ((res, sum), n) => if sum + n > 0 then (res + 1, sum + n) else return res }
      .pipe { case (res, _) => res }
