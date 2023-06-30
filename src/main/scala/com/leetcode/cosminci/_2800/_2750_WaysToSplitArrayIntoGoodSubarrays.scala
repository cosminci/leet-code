package com.leetcode.cosminci._2800

import scala.util.chaining.*

object _2750_WaysToSplitArrayIntoGoodSubarrays:

  def numberOfGoodSubarraySplits(nums: Array[Int]): Int =
    nums.indices.find(nums(_) == 1) match
      case None => 0
      case Some(start) =>
        Iterator
          .iterate((1L, 0, start)) { case (res, cnt, i) =>
            if nums(i) == 0 then (res, cnt + 1, i + 1)
            else ((res * (cnt + 1)) % 1_000_000_007, 0, i + 1)
          }
          .dropWhile { case (_, _, i) => i < nums.length }.next()
          .pipe { case (res, _, _) => res.toInt }
