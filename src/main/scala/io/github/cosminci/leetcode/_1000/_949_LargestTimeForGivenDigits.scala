package io.github.cosminci.leetcode._1000

object _949_LargestTimeForGivenDigits:
  def main(args: Array[String]): Unit =
    println(largestTimeFromDigits(Array(1, 2, 3, 4)))
    println(largestTimeFromDigits(Array(0, 0, 0, 0)))
    println(largestTimeFromDigits(Array(0, 4, 0, 0)))
    println(largestTimeFromDigits(Array(1, 9, 0, 6)))

  def largestTimeFromDigits(arr: Array[Int]): String =
    val candidates = arr.permutations
      .filter { case Array(hour1, hour2, min1, min2) =>
        hour1 * 10 + hour2 < 24 && min1 * 10 + min2 < 60
      }
    if candidates.isEmpty then return ""

    val Array(hour1, hour2, min1, min2) = candidates.maxBy { case Array(hour1, hour2, min1, min2) =>
      hour1 * 1000 + hour2 * 100 + min1 * 10 * min2 + min1
    }
    s"$hour1$hour2:$min1$min2"
