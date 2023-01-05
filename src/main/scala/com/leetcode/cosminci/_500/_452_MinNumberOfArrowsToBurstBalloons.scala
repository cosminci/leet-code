package com.leetcode.cosminci._500

object _452_MinNumberOfArrowsToBurstBalloons:

  def findMinArrowShots(points: Array[Array[Int]]): Int =
    points.sortBy(_.head).tail.foldLeft(Seq(points.minBy(_.head))) {
      case (all @ prev :+ Array(_, last), ball @ Array(l, r)) =>
        if (l > last) all :+ ball
        else prev :+ Array(l, r.min(last))
    }.length
