package com.leetcode.cosminci._2300

import scala.collection.Searching.*

object _2250_CountRectanglesContainingEachPoint:

  def countRectangles(rectangles: Array[Array[Int]], points: Array[Array[Int]]): Array[Int] =
    val sortedWidths = rectangles
      .foldLeft(Map.empty[Int, Array[Int]]) { case (acc, Array(width, height)) =>
        acc.updatedWith(height) {
          case None         => Some(Array(width))
          case Some(widths) => Some(widths :+ width)
        }
      }.view.mapValues(_.sorted).toMap

    points.map { case Array(x, y) =>
      sortedWidths.map { case (height, widths) =>
        if height < y then 0
        else
          widths.search(x) match
            case Found(idx)          => widths.length - idx
            case InsertionPoint(idx) => widths.length - idx
      }.sum
    }
