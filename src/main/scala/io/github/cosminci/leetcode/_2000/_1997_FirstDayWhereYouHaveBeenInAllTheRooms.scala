package io.github.cosminci.leetcode._2000

object _1997_FirstDayWhereYouHaveBeenInAllTheRooms:
  def main(args: Array[String]): Unit =
    println(firstDayBeenInAllRooms(Array(0, 0)))
    println(firstDayBeenInAllRooms(Array(0, 0, 2)))

  private def firstDayBeenInAllRooms(nextVisit: Array[Int]): Int =
    val mod = 1_000_000_007
    val dp  = Array.ofDim[Long](nextVisit.length)

    nextVisit.indices.tail.foreach { day =>
      dp(day) = (2 + 2 * dp(day - 1) - dp(nextVisit(day - 1)) + mod) % mod
    }

    (dp.last % mod).toInt
