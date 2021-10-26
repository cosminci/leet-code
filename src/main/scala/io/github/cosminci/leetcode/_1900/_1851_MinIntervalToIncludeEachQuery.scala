package io.github.cosminci.leetcode._1900

import scala.collection.mutable

object _1851_MinIntervalToIncludeEachQuery:
  def main(args: Array[String]): Unit =
    println(minInterval(Array(Array(1, 4), Array(2, 4), Array(3, 6), Array(4, 4)), Array(2, 3, 4, 5)).toList)

  def minInterval(inputIntervals: Array[Array[Int]], inputQueries: Array[Int]): Array[Int] =
    given Ordering[Interval] = (x, y) => x.start.compare(y.start)

    val intervals = inputIntervals.map { case Array(start, end) => Interval(start, end) }.sorted
    val queries   = inputQueries.zipWithIndex.sortBy(_._1)

    given Ordering[ActiveInterval] = (x, y) =>
      val lengthCompare = y.length.compareTo(x.length)
      if lengthCompare != 0 then lengthCompare else y.end.compareTo(x.end)
      
    val activeIntervals = mutable.PriorityQueue.empty[ActiveInterval]

    var intervalIdx = 0
    val result      = Array.ofDim[Int](inputQueries.length)
    queries.foreach { case (query, queryIdx) =>
      while intervalIdx < inputIntervals.length && intervals(intervalIdx).start <= query do
        val interval = intervals(intervalIdx)
        activeIntervals.enqueue(ActiveInterval(interval.end - interval.start + 1, interval.end))
        intervalIdx += 1
      while activeIntervals.headOption.exists(_.end < query) do activeIntervals.dequeue()
      val queryInterval = if activeIntervals.isEmpty then -1 else activeIntervals.head.length
      result(queryIdx) = queryInterval
    }

    result

  case class Interval(start: Int, end: Int)

  case class ActiveInterval(length: Int, end: Int)
