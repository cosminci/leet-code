package com.leetcode.cosminci._2600

import scala.util.chaining.*

object _2593_FindScoreOfArrayAfterMarkingAllElements:

  def findScore(nums: Array[Int]): Long =
    nums.zipWithIndex.sorted
      .foldLeft(Set.empty[Int], 0L) { case ((marked, res), (n, i)) =>
        if marked.contains(i) then (marked, res)
        else (marked ++ Seq((i - 1).max(0), i, (i + 1).min(nums.length - 1)), res + n)
      }
      .pipe { case (_, res) => res }
