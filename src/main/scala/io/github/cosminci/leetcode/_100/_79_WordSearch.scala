package io.github.cosminci.leetcode._100

import io.github.cosminci.utils

import scala.collection.mutable

object _79_WordSearch:
  def main(args: Array[String]): Unit =
    println(exist(Array(Array('a', 'b'), Array('c', 'd')), "acdb"))
    println(exist(Array(Array('a')), "a"))
    println(
      exist(
        Array(
          Array('A', 'B', 'C', 'E'),
          Array('S', 'F', 'C', 'S'),
          Array('A', 'D', 'E', 'E')
        ),
        "ABCCED"
      )
    )

  private def exist(board: Array[Array[Char]], word: String): Boolean =
    val visited = mutable.Set.empty[(Int, Int)]

    def dfs(i: Int, j: Int, wIdx: Int): Boolean =
      if visited.contains((i, j)) then return false
      visited.add((i, j))

      val result =
        if board(i)(j) != word(wIdx) then false
        else if wIdx == word.length - 1 then true
        else
          utils.neighbours(i, j, board).exists { case (i1, j1) =>
            dfs(i1, j1, wIdx + 1)
          }

      visited.remove((i, j))
      result

    board.indices.exists { i =>
      board(i).indices.exists { j =>
        dfs(i, j, 0)
      }
    }
