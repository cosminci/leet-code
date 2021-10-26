package io.github.cosminci.leetcode._100

import scala.annotation.tailrec

object _51_NQueens:
  def main(args: Array[String]): Unit =
    println(solveNQueens(7))

  private def solveNQueens(n: Int): List[List[String]] =
    @tailrec
    def solve(prevConfigs: List[List[(Int, Int)]]): List[List[(Int, Int)]] =
      if prevConfigs.isEmpty then return List.empty
      val newCol = prevConfigs.head.length
      if newCol == n then return prevConfigs

      val newConfigs = prevConfigs.flatMap { prevValidConfig =>
        (0 until n).map(i => (i, newCol)).collect {
          case newPosition if isValid(prevValidConfig, newPosition) =>
            prevValidConfig :+ newPosition
        }
      }
      solve(newConfigs)

    solve((0 until n).map(i => List((i, 0))).toList).map { queens =>
      val grid = Array.fill(n, n)('.')
      queens.foreach { case (x, y) => grid(x)(y) = 'Q' }
      grid.map(_.mkString).toList
    }

  private def isValid(prevValidConfig: List[(Int, Int)], newPosition: (Int, Int)) =
    val (x, y) = newPosition
    prevValidConfig.forall { case (prevX, prevY) =>
      prevX != x && prevY != y && (prevX - prevY) != (x - y) && (prevX + prevY) != (x + y)
    }
