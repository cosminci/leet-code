package com.leetcode.cosminci._300

import scala.util.chaining.*

object _209_MinSizeSubarraySum:

  def minSubArrayLen(target: Int, nums: Array[Int]): Int =
    nums.indices.foldLeft((Int.MaxValue, 0, 0)) { case ((res, sum, l), r) =>
      if nums(r) >= target then return 1
      Iterator
        .iterate((res, sum + nums(r), l)) { case (res, sum, l) => (res.min(r - l + 1), sum - nums(l), l + 1) }
        .dropWhile { case (_, sum, _) => sum >= target }.next()
    }.pipe { case (res, _, _) => if res == Int.MaxValue then 0 else res }
