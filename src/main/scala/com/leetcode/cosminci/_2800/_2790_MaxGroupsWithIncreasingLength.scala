package com.leetcode.cosminci._2800

import scala.util.chaining.*

object _2790_MaxGroupsWithIncreasingLength:

  def maxIncreasingGroups(usageLimits: List[Int]): Int =
    usageLimits.sorted.pipe {
      _.foldLeft(0L, 0) { case ((total, cnt), limit) =>
        if total + limit < (cnt + 1) * (cnt + 2) / 2 then (total + limit, cnt + 1)
        else (total + limit, cnt)
      }.pipe { case (_, cnt) => cnt }
    }
