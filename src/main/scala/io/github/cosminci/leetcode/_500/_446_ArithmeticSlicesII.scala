package io.github.cosminci.leetcode._500

import scala.collection.mutable

object _446_ArithmeticSlicesII:
  def main(args: Array[String]): Unit =
    println(numberOfArithmeticSlices(Array(0, 2000000000, -294967296)))
    println(numberOfArithmeticSlices(Array(2, 4, 6, 8, 10)))
    println(numberOfArithmeticSlices(Array(7, 7, 7, 7, 7)))

  def numberOfArithmeticSlices(nums: Array[Int]): Int =
    val mem    = mutable.Map.empty[(Int, Int), Int]
    var result = 0L

    nums.indices.foreach { i =>
      (0 until i).foreach { j =>
        val delta = nums(i).toLong - nums(j)
        if delta >= Int.MinValue && delta <= Int.MaxValue then
          val d      = delta.toInt
          val jCount = mem.getOrElse((j, d), 0)
          val iCount = mem.getOrElse((i, d), 0)
          mem.update((i, d), iCount + jCount + 1)
          result += jCount
      }
    }

    result.toInt
