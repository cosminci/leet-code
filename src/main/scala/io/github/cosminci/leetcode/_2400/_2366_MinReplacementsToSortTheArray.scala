package io.github.cosminci.leetcode._2400

import scala.math.Integral.Implicits.*

object _2366_MinReplacementsToSortTheArray:

  def minimumReplacement(nums: Array[Int]): Long =
    nums
      .foldRight(Int.MaxValue, 0L) { case (n, (right, cnt)) =>
        if n <= right then (n, cnt)
        else
          val div = (n - 1) / right + 1
          (n / div, cnt + div - 1)
      }
      ._2
