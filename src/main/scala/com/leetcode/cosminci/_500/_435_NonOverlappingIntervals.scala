package com.leetcode.cosminci._500

object _435_NonOverlappingIntervals:
  def main(args: Array[String]): Unit =
    println(eraseOverlapIntervals(Array(Array(0, 2), Array(1, 3), Array(2, 4), Array(3, 5), Array(4, 6))))
    println(eraseOverlapIntervals(Array(Array(1, 2), Array(2, 3), Array(3, 4), Array(1, 3))))
    println(eraseOverlapIntervals(Array(Array(1, 2), Array(1, 2), Array(1, 2))))
    println(eraseOverlapIntervals(Array(Array(1, 2), Array(2, 3))))

  def eraseOverlapIntervals(intervals: Array[Array[Int]]): Int =
    intervals.sortInPlaceBy(_.last)

    var deleteCount = 0
    intervals.indices.tail.foldLeft(intervals.head) { case (prev, i) =>
      if intervals(i).head < prev.last then
        deleteCount += 1
        prev
      else intervals(i)
    }

    deleteCount
