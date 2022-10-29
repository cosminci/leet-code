package com.leetcode.cosminci._1600

import scala.util.chaining.*

object _1578_MinTimeToMakeRopeColorful:

  def minCost(colors: String, neededTime: Array[Int]): Int =
    (colors :+ 'X', neededTime :+ 0).pipe { case (colors, neededTime) =>
      colors.indices.tail
        .foldLeft(0, neededTime.head, neededTime.head) { case ((cost, sum, max), i) =>
          if colors(i) == colors(i - 1) then (cost, sum + neededTime(i), max.max(neededTime(i)))
          else (cost + sum - max, neededTime(i), neededTime(i))
        }
        ._1
    }
