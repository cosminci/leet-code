package com.leetcode.cosminci._400

object _391_PerfectRectangle:

  def isRectangleCover(rectangles: Array[Array[Int]]): Boolean = {
    val (xMin, yMin, xMax, yMax) = rectangles.foldLeft(Int.MaxValue, Int.MaxValue, Int.MinValue, Int.MinValue) {
      case ((xMin, yMin, xMax, yMax), Array(x1, y1, x2, y2)) =>
        (xMin.min(x1), yMin.min(y1), xMax.max(x2), yMax.max(y2))
    }
    val corners = Set((xMin, yMin), (xMin, yMax), (xMax, yMin), (xMax, yMax))

    val (counter, area) = rectangles.foldLeft(Map.empty[(Int, Int), Int], 0) {
      case ((counter, area), Array(x1, y1, x2, y2)) =>
        (Seq((x1, y1), (x1, y2), (x2, y1), (x2, y2)).foldLeft(counter) { (counter, corner) =>
          counter.updated(corner, counter.getOrElse(corner, 0) + 1)
        }, area + (x2 - x1) * (y2 - y1))
    }
    counter.toSeq.collect { case (corner, count) if count % 2 == 1 => corner }.toSet == corners &&
      area == (xMax - xMin) * (yMax - yMin)
  }
