package io.github.cosminci.leetcode._500

import scala.collection.mutable

object _452_MinNumberOfArrowsToBurstBalloons:
  def main(args: Array[String]): Unit =
    println(findMinArrowShots(Array(Array(10, 16), Array(2, 8), Array(1, 6), Array(7, 12))))
    println(findMinArrowShots(Array(Array(1, 2), Array(3, 4), Array(5, 6), Array(7, 8))))
    println(findMinArrowShots(Array(Array(1, 2), Array(2, 3), Array(3, 4), Array(4, 5))))

  def findMinArrowShots(points: Array[Array[Int]]): Int =
    points.sortInPlaceBy(_.head)
    val overlaps = mutable.Stack(points.head)
    points.tail.foreach { b =>
      if b.head > overlaps.head.last then overlaps.push(b)
      else overlaps.push(Array(b.head, math.min(b.last, overlaps.pop().last)))
    }
    overlaps.size
