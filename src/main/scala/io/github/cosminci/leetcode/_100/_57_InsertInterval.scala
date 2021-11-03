package io.github.cosminci.leetcode._100

import scala.collection.mutable

object _57_InsertInterval:
  def main(args: Array[String]): Unit =
    println(
      insert(Array(Array(1, 3), Array(6, 9)), Array(2, 5)).map(_.toList).toList
    )

  def insert(
      intervals: Array[Array[Int]],
      newInterval: Array[Int]
  ): Array[Array[Int]] =
    if intervals.isEmpty then return Array(newInterval)
    if newInterval(1) < intervals.head(0) then return newInterval +: intervals
    if newInterval(0) > intervals.last(1) then return intervals :+ newInterval

    val result = mutable.ListBuffer.empty[Array[Int]]

    var Array(newStart, newEnd) = newInterval
    intervals.zipWithIndex.foreach { case (i @ Array(start, end), idx) =>
      if end < newStart then result.addOne(i)
      else if start > newEnd then
        return result.addOne(Array(newStart, newEnd)).toArray ++ intervals
          .slice(idx, intervals.length)
      else
        newStart = math.min(start, newStart)
        newEnd = math.max(end, newEnd)
    }
    result.addOne(Array(newStart, newEnd)).toArray
