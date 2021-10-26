package io.github.cosminci.leetcode._600

import scala.collection.mutable

object _576_OutOfBoundaryPaths:
  def main(args: Array[String]): Unit =
    println(findPaths(2, 2, 2, 0, 0))
    println(findPaths(1, 3, 3, 0, 1))

  private def findPaths(m: Int, n: Int, maxMove: Int, startRow: Int, startColumn: Int): Int =
    val mem = mutable.Map.empty[(Int, Int, Int), Long]
    def dfs(r: Int, c: Int, moves: Int): Long =
      mem.getOrElseUpdate((r, c, moves), {
        if r < 0 || r == m || c < 0 || c == n then 1
        else if moves == 0 then 0
        else (
          dfs(r - 1, c, moves - 1) +
          dfs(r + 1, c, moves - 1) +
          dfs(r, c - 1, moves - 1) +
          dfs(r, c + 1, moves - 1)
          ) % 1_000_000_007
      })

    dfs(startRow, startColumn, maxMove).toInt
