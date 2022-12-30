package com.leetcode.cosminci._400

import scala.collection.mutable

object _352_DataStreamAsDisjointIntervals:

  class SummaryRanges:
    private val tree = mutable.TreeMap.empty[Int, Range]

    def addNum(value: Int): Unit =
      if !tree.contains(value) then
        (tree.maxBefore(value), tree.minAfter(value + 1)) match
          case (Some((low, rangeLow)), Some((high, rangeHigh))) if value == rangeLow.end + 1 && value == high - 1 =>
            tree.remove(high)
            tree.addOne((low, rangeLow.start to rangeHigh.end))
          case (_, Some((high, rangeHigh))) if value == high - 1 =>
            tree.remove(high)
            tree.addOne((value, value to rangeHigh.end))
          case (Some((low, rangeLow)), _) if value == rangeLow.end + 1 =>
            tree.addOne((low, rangeLow.start to value))
          case (Some((low, rangeLow)), _) if value <= rangeLow.end =>
          case _ =>
            tree.addOne((value, value to value))

    def getIntervals(): Array[Array[Int]] =
      tree.values.map(range => Array(range.start, range.end)).toArray
