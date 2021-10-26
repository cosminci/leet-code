package io.github.cosminci.leetcode._100

import scala.collection.mutable

object _56_MergeIntervals {
  def main(args: Array[String]): Unit = {
    println(merge(Array(Array(1,4), Array(2,3))).map(_.toList).toList)
    println(merge(Array(Array(1,4), Array(4,5), Array(4, 10), Array(11,13), Array(11, 15))).map(_.toList).toList)
  }

  def merge(intervals: Array[Array[Int]]): Array[Array[Int]] = {
    given Ordering[Array[Int]] = (x, y) => x.head.compareTo(y.head)
    intervals.sortInPlace

    val result = mutable.Stack(intervals.head)
    intervals.tail.foreach {
      case interval @ Array(start, end) =>
        if (start > result.head.last)
          result.push(interval)
        else {
          val prev = result.pop()
          result.push(Array(prev.head, math.max(prev.last, end)))
        }
    }
    result.popAll().toArray
  }
}
