package io.github.cosminci.leetcode._2100

import scala.collection.immutable.TreeMap

object _2007_FindOriginalArrayFromDoubledArray:
  def main(args: Array[String]): Unit =
    println(findOriginalArray(Array(1, 3, 4, 2, 6, 8, 8, 4, 2, 1)).toSeq)
    println(findOriginalArray(Array(6, 3, 0, 1)).toSeq)
    println(findOriginalArray(Array(1)).toSeq)
    println(findOriginalArray(Array(0)).toSeq)
    println(findOriginalArray(Array(0, 0, 0, 0)).toSeq)

  def findOriginalArray(changed: Array[Int]): Array[Int] = {
    val freqCounts = TreeMap.from(changed.groupBy(identity).view.mapValues(_.length)).withDefaultValue(0)
    if freqCounts(0) % 2 != 0 then return Array.empty

    freqCounts.foldLeft(freqCounts) {
      case (remainingFreq, (n, freq)) =>
        if remainingFreq(n) > remainingFreq(2 * n) then return Array.empty
        val freqDrop = if n != 0 then remainingFreq(n) else remainingFreq(n) / 2
        remainingFreq.updated(2 * n, remainingFreq(2 * n) - freqDrop)
    }.flatMap {
      case (n, count) =>
        Seq.fill(count)(n)
    }.toArray
  }
