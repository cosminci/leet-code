package com.leetcode.cosminci._2900

import scala.collection.mutable

object _2801_CountSteppingNumsInRange:

  def countSteppingNumbers(low: String, high: String): Int =
    val leadingZeroes         = high.length - low.length
    val (lowLimit, highLimit) = (Seq.fill(leadingZeroes)('0').mkString ++ low, high)

    val mem = mutable.Map.empty[(Int, Int, Boolean, Boolean), Long]
    def dfs(i: Int, prev: Int, lowerBound: Boolean, upperBound: Boolean): Long =
      mem.getOrElseUpdate((i, prev, lowerBound, upperBound),
        if i == highLimit.length then 1L
        else (0 to 9).foldLeft(0L) { (cnt, d) =>
          val fillZero = d == 0 && lowerBound && i < leadingZeroes
          if prev == -1 || (d - prev).abs == 1 || fillZero then
            if lowerBound && d < lowLimit(i) - '0' then cnt
            else if upperBound && d > highLimit(i) - '0' then cnt
            else
              val newLowerBound = lowerBound && d == (lowLimit(i) - '0')
              val newUpperBound = upperBound && d == (highLimit(i) - '0')
              val newPrev       = if fillZero then -1 else d
              (cnt + dfs(i + 1, newPrev, newLowerBound, newUpperBound)) % 1_000_000_007
          else cnt
        }
      )

    dfs(i = 0, prev = -1, lowerBound = true, upperBound = true).toInt
