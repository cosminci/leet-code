package io.github.cosminci.leetcode._1000

import scala.collection.mutable

object _939_MinAreaRectangle:
  def main(args: Array[String]): Unit =
    println(minAreaRect(Array(Array(1, 1), Array(1, 3), Array(3, 1), Array(3, 3), Array(2, 2))))
    println(minAreaRect(Array(Array(1, 1), Array(1, 3), Array(3, 1), Array(3, 3), Array(4, 1), Array(4, 3))))

  private def minAreaRect(points: Array[Array[Int]]): Int =
    val seen    = mutable.Set.empty[(Int, Int)]
    var minArea = Int.MaxValue

    points.foreach { case Array(x1, y1) =>
      seen.foreach { case (x2, y2) =>
        if seen.contains((x1, y2)) && seen.contains((x2, y1)) then
          minArea = math.min(minArea, math.abs(x2 - x1) * math.abs(y2 - y1))
      }
      seen.add((x1, y1))
    }

    if minArea != Int.MaxValue then minArea else 0
