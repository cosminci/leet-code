package com.leetcode.cosminci._100

import scala.util.chaining.*

object _57_InsertInterval:

  def insert(intervals: Array[Array[Int]], newInterval: Array[Int]): Array[Array[Int]] =
    intervals
      .foldLeft((Array.empty[Array[Int]], newInterval)) {
        case ((prev, newInterval @ Array(newL, newR)), last @ Array(prevL, prevR)) =>
          if newR < prevL then (prev :+ newInterval, last)
          else if newL > prevR then (prev :+ last, newInterval)
          else (prev, Array(prevL min newL, prevR max newR))
      }
      .pipe { case (acc, last) => acc :+ last }
