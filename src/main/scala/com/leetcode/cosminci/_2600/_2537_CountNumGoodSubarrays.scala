package com.leetcode.cosminci._2600

import scala.util.chaining.*

object _2537_CountNumGoodSubarrays:

  def countGood(nums: Array[Int], k: Int): Long =
    nums.zipWithIndex
      .foldLeft(Map.empty[Int, Int].withDefaultValue(0), 0, 0, 0L) { case ((counts, pairs, start, result), (n, j)) =>
        Iterator
          .iterate((counts.updated(n, counts(n) + 1), pairs + counts(n), start, result)) {
            case (counts, pairs, i, result) => (
              counts.updated(nums(i), counts(nums(i)) - 1),
              pairs - (counts(nums(i)) - 1),
              i + 1,
              result + (nums.length - j)
            )
          }
          .dropWhile { case (_, pairs, _, _) => pairs >= k }.next()
      }
    .pipe { case (_, _, _, result) => result }
