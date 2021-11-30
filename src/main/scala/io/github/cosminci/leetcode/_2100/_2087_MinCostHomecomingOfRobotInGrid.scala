package io.github.cosminci.leetcode._2100

object _2087_MinCostHomecomingOfRobotInGrid:
  def main(args: Array[String]): Unit =
    println(
      minCost(
        Array(5, 5),
        Array(5, 2),
        Array(7, 1, 3, 3, 5, 3, 22, 10, 23),
        Array(5, 5, 6, 2, 0, 16)
      )
    )

  def minCost(start: Array[Int], home: Array[Int], rowCosts: Array[Int], colCosts: Array[Int]): Int = {
    val rows = Option.when(start(0) < home(0))(start(0) + 1 to home(0)).getOrElse(home(0) until start(0))
    val cols = Option.when(start(1) < home(1))(start(1) + 1 to home(1)).getOrElse(home(1) until start(1))
    rows.map(rowCosts).sum + cols.map(colCosts).sum
  }
