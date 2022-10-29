package com.leetcode.cosminci._200

object _149_MaxPointsOnALine:

  def maxPoints(points: Array[Array[Int]]): Int =
    def slope(i: Int, j: Int) =
      val (Array(x0, y0), Array(x1, y1)) = (points(i), points(j))
      if y0 == y1 then Int.MaxValue else (x0 - x1) * 1.0 / (y0 - y1)

    (0 until points.length - 1).foldLeft(0) { (globalMax, i) =>
      (i + 1 until points.length)
        .groupMapReduce(j => slope(i, j))(_ => 1)(_ + _)
        .values
        .maxOption.getOrElse(0)
        .max(globalMax)
    } + 1
