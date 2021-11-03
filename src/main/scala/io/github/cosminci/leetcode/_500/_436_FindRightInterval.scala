package io.github.cosminci.leetcode._500

import scala.collection.mutable

object _436_FindRightInterval:
  def main(args: Array[String]): Unit =
    println(findRightInterval(Array(Array(3, 4), Array(2, 3), Array(1, 2))).toSeq)
    println(findRightInterval(Array(Array(1, 4), Array(2, 3), Array(3, 4))).toSeq)

  def findRightInterval(intervals: Array[Array[Int]]): Array[Int] =
    val startToIdx = mutable.TreeMap.from(
      intervals.zipWithIndex.map { case (Array(start, _), idx) =>
        start -> idx
      }
    )

    intervals.map { case Array(start, end) =>
      startToIdx
        .minAfter(end)
        .map { case (_, idx) =>
          idx
        }
        .getOrElse(-1)
    }
