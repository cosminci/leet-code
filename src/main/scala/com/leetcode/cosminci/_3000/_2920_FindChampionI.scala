package com.leetcode.cosminci._3000

object _2920_FindChampionI:

  def findChampion(grid: Array[Array[Int]]): Int =
    grid.indices.find(i => grid.indices.forall(j => grid(j)(i) == 0)).getOrElse(-1)
