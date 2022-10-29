package com.leetcode.cosminci._100

import scala.collection.mutable

object _56_MergeIntervals {
  def main(args: Array[String]): Unit = {
    println(merge(Array(Array(1,4), Array(2,3))).map(_.toList).toList)
    println(merge(Array(Array(1,4), Array(4,5), Array(4, 10), Array(11,13), Array(11, 15))).map(_.toList).toList)
  }

  def merge(intervals: Array[Array[Int]]): Array[Array[Int]] = {
    @annotation.tailrec
    def merge(merged: Array[Array[Int]], interval: Array[Int]): Array[Array[Int]] =
      if (merged.isEmpty) Array(interval)
      else if (interval.head > merged.last.last) merged :+ interval
      else merge(merged.dropRight(1), Array(merged.last.head, merged.last.last.max(interval.last)))

    intervals.sortBy(_.head).foldLeft(Array.empty[Array[Int]])(merge)
  }
}
