package io.github.cosminci.leetcode._100

object _51_NQueens:
  def main(args: Array[String]): Unit =
    println(solveNQueens(7))

  def solveNQueens(n: Int): List[List[String]] =
    @annotation.tailrec
    def solve(prevConfigs: List[List[(Int, Int)]]): List[List[(Int, Int)]] =
      if prevConfigs.isEmpty then List.empty
      else if prevConfigs.head.length == n then prevConfigs
      else
        val newConfigs = for
          prevConfig <- prevConfigs
          row        <- 0 until n
          col = prevConfigs.head.length
          if isValid(prevConfig, row, col)
        yield prevConfig :+ (row, col)
        solve(newConfigs)

    solve((0 until n).map(row => List((row, 0))).toList).map { queens =>
      Array.tabulate(n, n) { case pos => if queens.contains(pos) then 'Q' else '.' }.map(_.mkString).toList
    }

  private def isValid(prevValidConfig: List[(Int, Int)], x: Int, y: Int) =
    prevValidConfig.forall { case (prevX, prevY) =>
      prevX != x && prevY != y && (prevX - prevY) != (x - y) && (prevX + prevY) != (x + y)
    }
