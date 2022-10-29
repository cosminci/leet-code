package com.leetcode.cosminci._2100

import scala.collection.mutable

object _2054_TwoBestNonOverlappingEvents:
  def maxTwoEvents(events: Array[Array[Int]]): Int =
    events.sortInPlaceBy(_.head)

    val inProgress = mutable.PriorityQueue.empty[Array[Int]]((x, y) => y(1).compare(x(1)))

    var (overallMax, completeMax) = (0, 0)
    events.foreach {
      case ev @ Array(start, _, value) =>
        while inProgress.nonEmpty && inProgress.head(1) < start do
          completeMax = completeMax.max(inProgress.dequeue()(2))
        overallMax = overallMax.max(completeMax + value)
        inProgress.enqueue(ev)
    }

    overallMax
