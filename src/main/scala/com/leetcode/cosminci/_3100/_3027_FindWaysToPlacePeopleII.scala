package com.leetcode.cosminci._3100

import scala.util.chaining.*

object _3027_FindWaysToPlacePeopleII:

  def numberOfPairs(points: Array[Array[Int]]): Int =
    val heights = points.sortBy { case Array(x, y) => (x, -y) }.map { case Array(_, y) => y }
    heights.zipWithIndex.foldLeft(0) { case (res, (y1, i)) =>
      (i + 1 until heights.length).map(heights)
        .foldLeft(res, Int.MinValue) { case ((res, y), y2) => if y1 >= y2 && y2 > y then (res + 1, y2) else (res, y) }
        .pipe { case (res, _) => res }
    }
