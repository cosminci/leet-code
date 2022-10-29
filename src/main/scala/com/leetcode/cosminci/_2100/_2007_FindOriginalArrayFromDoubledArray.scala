package com.leetcode.cosminci._2100

import scala.collection.immutable.TreeMap

object _2007_FindOriginalArrayFromDoubledArray:

  def findOriginalArray(changed: Array[Int]): Array[Int] =
    val freqCounts = TreeMap.from(changed.groupMapReduce(identity)(_ => 1)(_ + _)).withDefaultValue(0)
    if freqCounts(0) % 2 != 0 then return Array.empty

    freqCounts.keys
      .foldLeft(freqCounts) { (remainingFreq, n) =>
        if remainingFreq(n) > remainingFreq(2 * n) then return Array.empty
        val freqDrop = if n != 0 then remainingFreq(n) else remainingFreq(n) / 2
        remainingFreq.updated(2 * n, remainingFreq(2 * n) - freqDrop)
      }
      .flatMap { (n, count) => Seq.fill(count)(n) }
      .toArray
