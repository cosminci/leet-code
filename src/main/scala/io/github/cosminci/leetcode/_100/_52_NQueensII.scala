package io.github.cosminci.leetcode._100

object _52_NQueensII:

  def totalNQueens(n: Int): Int =
    def dfs(col: Int, colMask: Int, diag1Mask: Int, diag2Mask: Int): Int =
      if col == n then 1
      else
        (0 until n).map { row =>
          if (colMask & 1 << row) > 0 then 0
          else if (diag1Mask & 1 << (col - row + n)) > 0 then 0
          else if (diag2Mask & 1 << (col + row)) > 0 then 0
          else dfs(col + 1, colMask ^ 1 << row, diag1Mask ^ 1 << (col - row + n), diag2Mask ^ 1 << (col + row))
        }.sum

    dfs(col = 0, colMask = 0, diag1Mask = 0, diag2Mask = 0)
