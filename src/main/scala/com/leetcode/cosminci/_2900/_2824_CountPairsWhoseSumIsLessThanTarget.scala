package com.leetcode.cosminci._2900

import scala.util.chaining.*

object _2824_CountPairsWhoseSumIsLessThanTarget:

  def countPairs(nums: List[Int], target: Int): Int =
    nums.sorted.pipe { nums =>
      Iterator
        .iterate((nums.indices.head, nums.indices.last, 0)) { case (l, r, count) =>
          if nums(l) + nums(r) >= target then (l, r - 1, count)
          else (l + 1, r, count + r - l)
        }
        .dropWhile { case (l, r, _) => l < r }.next()
        .pipe { case (_, _, cnt) => cnt }
    }
