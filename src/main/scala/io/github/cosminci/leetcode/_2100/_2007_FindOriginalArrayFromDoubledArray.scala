package io.github.cosminci.leetcode._2100

import scala.collection.mutable

object _2007_FindOriginalArrayFromDoubledArray:
  def main(args: Array[String]): Unit =
    println(findOriginalArray(Array(1, 3, 4, 2, 6, 8, 8, 4, 2, 1)).toSeq)
    println(findOriginalArray(Array(6, 3, 0, 1)).toSeq)
    println(findOriginalArray(Array(1)).toSeq)
    println(findOriginalArray(Array(0)).toSeq)
    println(findOriginalArray(Array(0, 0, 0, 0)).toSeq)

  def findOriginalArray(changed: Array[Int]): Array[Int] =
    val freqCounts = mutable.TreeMap.from(changed.groupBy(identity).mapValues(_.length)).withDefaultValue(0)
    if freqCounts(0) % 2 != 0 then return Array.empty

    freqCounts.foreach { case (n, freq) =>
      if freqCounts(n) > freqCounts(2 * n) then return Array.empty
      val freqDrop = if n != 0 then freqCounts(n) else freqCounts(n) / 2
      freqCounts.update(2 * n, freqCounts(2 * n) - freqDrop)
    }

    freqCounts.map { case (n, count) => Seq.fill(count)(n) }.flatten.toArray
