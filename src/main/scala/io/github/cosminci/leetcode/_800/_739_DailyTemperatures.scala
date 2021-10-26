package io.github.cosminci.leetcode._800

import scala.collection.mutable

object _739_DailyTemperatures:
  def main(args: Array[String]): Unit =
    println(dailyTemperatures(Array(73, 74, 75, 71, 69, 72, 76, 73)).toList)

  private def dailyTemperatures(temperatures: Array[Int]): Array[Int] =
    val decreasingTemp = mutable.Stack.empty[(Int, Int)]
    val result         = Array.ofDim[Int](temperatures.length)
    temperatures.indices.foreach { i =>
      while decreasingTemp.headOption.exists(_._1 < temperatures(i)) do
        val (_, idx) = decreasingTemp.pop()
        result(idx) = i - idx
      decreasingTemp.push((temperatures(i), i))
    }
    result
