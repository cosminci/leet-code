package io.github.cosminci.leetcode._2000

import scala.collection.mutable

object _1981_MinimizeTheDifferenceBetweenTargetAndChosenElements:
  def main(args: Array[String]): Unit =
    println(minimizeTheDifference(Array(Array(1, 1), Array(8, 9)), 10))

  def minimizeTheDifference(mat: Array[Array[Int]], target: Int): Int =
    val min = mat.map(_.min).sum
    if min >= target then return min - target
    val minDelta = target - min

    mat
      .foldLeft(Set(0)) { case (prevSums, row) =>
        for
          prevSum <- prevSums
          num     <- row
          if (prevSum + num - target <= minDelta)
        yield prevSum + num
      }
      .map(sum => math.abs(sum - target))
      .min
