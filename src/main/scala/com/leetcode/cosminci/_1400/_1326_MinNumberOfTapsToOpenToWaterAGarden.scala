package com.leetcode.cosminci._1400

import scala.collection.mutable

object _1326_MinNumberOfTapsToOpenToWaterAGarden:
  def main(args: Array[String]): Unit =
    println(minTaps(7, Array(1, 2, 1, 0, 2, 1, 0, 1)))

  def minTaps(n: Int, ranges: Array[Int]): Int =
    val intervals = ranges.indices.map { rangeCenter =>
      Interval(
        math.max(0, rangeCenter - ranges(rangeCenter)),
        math.min(rangeCenter + ranges(rangeCenter), ranges.length - 1)
      )
    }

    val intervalsLeft  = mutable.Queue.from(intervals.sortBy(_.start))
    val intervalsAdded = mutable.Stack.empty[Interval]

    while intervalsAdded.headOption.forall(_.end < n) do
      val currentEnd = intervalsAdded.headOption.map(_.end).getOrElse(0)
      intervalsLeft.dequeueWhile(_.start <= currentEnd).maxByOption(_.end) match
        case None =>
          return -1
        case Some(clip) =>
          intervalsAdded.push(clip)

    intervalsAdded.size

  private case class Interval(start: Int, end: Int)
