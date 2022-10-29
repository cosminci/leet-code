package com.leetcode.cosminci._2100

import scala.collection.mutable

object _2013_DetectSquares:
  def main(args: Array[String]): Unit =
    val detector = new DetectSquares
    detector.add(Array(3, 10))
    detector.add(Array(11, 2))
    detector.add(Array(3, 2))
    println(detector.count(Array(11, 10)))

  class DetectSquares:
    private val points = mutable.Map.empty[(Int, Int), Int]

    def add(point: Array[Int]): Unit =
      val Array(x, y) = point
      points.update((x, y), points.getOrElse((x, y), 0) + 1)

    def count(point: Array[Int]): Int =
      val Array(x1, y1) = point
      points.collect {
        case ((x3, y3), count) if x3 != x1 && math.abs(x3 - x1) == math.abs(y3 - y1) =>
          count * points.getOrElse((x1, y3), 0) * points.getOrElse((x3, y1), 0)
      }.sum
