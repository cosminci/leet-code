package io.github.cosminci.leetcode._2200

object _2110_NumberOfSmoothDescentPeriodsOfAStock:
  def main(args: Array[String]): Unit =
    println(getDescentPeriods(Array(3, 2, 1, 4)))

  def getDescentPeriods(prices: Array[Int]): Long =
    (prices :+ Int.MaxValue)
      .foldLeft(0L, 0, prices.head) { case ((count, length, prevPrice), currPrice) =>
        if currPrice == prevPrice - 1 then (count, length + 1, currPrice)
        else (count + (length.toLong * (length + 1) / 2), 1, currPrice)
      }._1
