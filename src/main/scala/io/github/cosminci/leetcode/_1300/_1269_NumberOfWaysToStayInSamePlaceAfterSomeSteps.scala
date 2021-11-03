package io.github.cosminci.leetcode._1300

object _1269_NumberOfWaysToStayInSamePlaceAfterSomeSteps:
  def main(args: Array[String]): Unit =
    println(numWays(3, 2))
    println(numWays(4, 2))
    println(numWays(27, 7))

  def numWays(steps: Int, arrLen: Int): Int =
    val (minPos, maxPos) = (0, math.min(steps / 2, arrLen - 1))

    val dp = Array.ofDim[Long](steps + 1, maxPos + 1)
    dp(0)(0) = 1

    (1 to steps).foreach { s =>
      (0 to maxPos).foreach { p =>
        val leftPos  = if p > 0 then dp(s - 1)(p - 1) else 0
        val rightPos = if p < maxPos then dp(s - 1)(p + 1) else 0
        dp(s)(p) = (dp(s - 1)(p) + leftPos + rightPos) % 1_000_000_007
      }
    }

    dp(steps)(0).toInt
