package com.leetcode.cosminci._2600

object _2556_DisconnectPathInBinaryMatrixByMaxOneFlip:

  def isPossibleToCutPath(grid: Array[Array[Int]]): Boolean =
    def dfs(i: Int, j: Int): Boolean =
      if i == grid.length - 1 && j == grid(i).length - 1 then true
      else if i == grid.length || j == grid(i).length || grid(i)(j) == 0 then false
      else
        if i != 0 || j != 0 then grid(i)(j) = 0
        dfs(i + 1, j) || dfs(i, j + 1)

    !dfs(i = 0, j = 0) || !dfs(i = 0, j = 0)
