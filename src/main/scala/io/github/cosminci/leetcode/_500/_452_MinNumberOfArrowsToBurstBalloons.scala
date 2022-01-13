package io.github.cosminci.leetcode._500

import scala.collection.mutable

object _452_MinNumberOfArrowsToBurstBalloons:
  def main(args: Array[String]): Unit =
    println(findMinArrowShots(Array(Array(10, 16), Array(2, 8), Array(1, 6), Array(7, 12))))
    println(findMinArrowShots(Array(Array(1, 2), Array(3, 4), Array(5, 6), Array(7, 8))))
    println(findMinArrowShots(Array(Array(1, 2), Array(2, 3), Array(3, 4), Array(4, 5))))

  def findMinArrowShots(points: Array[Array[Int]]): Int =
    points.sortBy(_(0)).tail.foldLeft(Seq(points.minBy(_(0)))) {
      case (all @ prev :+ last, b) =>
        if (b(0) > last(1)) all :+ b
        else prev :+ Array(b(0), b(1).min(last(1)))
    }.length
