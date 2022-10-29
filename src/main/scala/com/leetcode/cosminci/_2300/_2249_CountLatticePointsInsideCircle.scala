package com.leetcode.cosminci._2300

import scala.collection.mutable

object _2249_CountLatticePointsInsideCircle:

  def countLatticePoints(circles: Array[Array[Int]]): Int =
    val points = mutable.Set.empty[(Int, Int)]
    for
      Array(x, y, r) <- circles
      dx             <- 0 to r
      dy             <- 0 to math.sqrt(math.pow(r, 2) - math.pow(dx, 2)).toInt
    do
      points.add((x + dx, y + dy))
      points.add((x + dx, y - dy))
      points.add((x - dx, y + dy))
      points.add((x - dx, y - dy))
    points.size
