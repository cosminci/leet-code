package io.github.cosminci.leetcode._2300

import scala.math.Integral.Implicits.*

object _2224_MinNumOfOpsToConvertTime:

  def convertTime(current: String, correct: String): Int =
    Array(60, 15, 5, 1)
      .foldLeft(0, (minutes(current) - minutes(correct)).abs) { case ((ops, minutesLeft), unit) =>
        (ops + minutesLeft / unit, minutesLeft % unit)
      }._1

  private def minutes(s: String) = s.take(2).toInt * 60 + s.takeRight(2).toInt
