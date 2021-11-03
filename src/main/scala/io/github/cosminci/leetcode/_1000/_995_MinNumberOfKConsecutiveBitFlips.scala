package io.github.cosminci.leetcode._1000

import scala.collection.mutable

object _995_MinNumberOfKConsecutiveBitFlips:
  def main(args: Array[String]): Unit =
    println(minKBitFlips(Array(0, 0, 0, 1, 0, 1, 1, 0), 3))

  def minKBitFlips(nums: Array[Int], k: Int): Int =
    val flipsWindow = mutable.Queue.empty[Int]
    var flipCount   = 0

    nums.indices.foreach { i =>
      val expectedValue = if flipsWindow.size % 2 == 1 then 0 else 1
      if nums(i) != expectedValue then
        flipCount += 1
        flipsWindow.enqueue(i + k - 1)
      if flipsWindow.headOption.exists(_ <= i) then flipsWindow.dequeue()
    }

    if flipsWindow.isEmpty then flipCount else -1
