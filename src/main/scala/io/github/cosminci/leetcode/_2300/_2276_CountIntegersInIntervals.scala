package io.github.cosminci.leetcode._2300

import scala.annotation.tailrec
import scala.collection.mutable

object _2276_CountIntegersInIntervals:

  class CountIntervals:
    private val nonOverlapping = mutable.TreeMap.empty[Int, Int]
    private var integerCount   = 0

    def add(left: Int, right: Int): Unit =
      val (finalLeft, finalRight) = removeOverlapping(left, right)
      nonOverlapping.put(finalLeft, finalRight)
      integerCount += finalRight - finalLeft + 1

    def count(): Int = integerCount

    @tailrec
    private def removeOverlapping(left: Int, right: Int): (Int, Int) =
      nonOverlapping.maxBefore(right + 1) match
        case None => (left, right)
        case Some((_, prevRight)) if prevRight < left => (left, right)
        case Some((prevLeft, prevRight)) =>
          integerCount -= prevRight - prevLeft + 1
          nonOverlapping.remove(prevLeft)
          removeOverlapping(prevLeft.min(left), prevRight.max(right))
