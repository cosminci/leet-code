package io.github.cosminci.leetcode._800

import scala.collection.mutable

object _799_ChampagneTower:
  def main(args: Array[String]): Unit =
    println(champagneTowerTopDown(4, 3, 3))
    println(champagneTowerBottomUp(4, 3, 3))
    println(champagneTowerTopDown(25, 6, 1))
    println(champagneTowerBottomUp(25, 6, 1))

  def champagneTowerTopDown(poured: Int, queryRow: Int, queryGlass: Int): Double =
    val mem = mutable.Map.empty[(Int, Int), Double]

    def dfs(row: Int, col: Int): Double =
      mem.getOrElseUpdate((row, col), {
        if row == 0 then poured
        else if col == 0 then (dfs(row - 1, col) - 1).max(0) / 2
        else if col == row then (dfs(row - 1, col - 1) - 1).max(0) / 2
        else (dfs(row - 1, col) - 1).max(0) / 2 + (dfs(row - 1, col - 1) - 1).max(0) / 2
      })

    dfs(queryRow, queryGlass).min(1)

  def champagneTowerBottomUp(poured: Int, queryRow: Int, queryGlass: Int): Double =
    (1 to queryRow).foldLeft(Seq(poured.toDouble)) { (prevRow, row) =>
        (1 until prevRow.length)
          .map(i => (prevRow(i - 1) - 1).max(0) / 2 + (prevRow(i) - 1).max(0) / 2)
          .prepended((prevRow.head - 1).max(0) / 2)
          .appended((prevRow.last - 1).max(0) / 2)
      }(queryGlass).min(1)
