package io.github.cosminci.leetcode._1600

import io.github.cosminci.utils

import scala.collection.mutable

object _1553_MinNumberOfDaysToEatNOranges:

  private val mem = mutable.Map[Int, Int](0 -> 0, 1 -> 1)

  def main(args: Array[String]): Unit =
    utils.timeSolution("Bottom up", () => minDaysBottomUp(61455274))
    utils.timeSolution("Top down", () => minDaysTopDown(61455274))

  private def minDaysBottomUp(n: Int): Int =
    val dp = Array.ofDim[Int](n + 1)
    dp(1) = 1
    (2 to n).foreach { day =>
      dp(day) = 1 + math.min(day % 3 + dp(day / 3), day % 2 + dp(day / 2))
    }
    dp(n)

  private def minDaysTopDown(n: Int): Int =
    if mem.contains(n) then return mem(n)
    val result = 1 + math.min(n % 3 + minDaysTopDown(n / 3), n % 2 + minDaysTopDown(n / 2))
    mem.update(n, result)
    result
