package com.leetcode.cosminci._2700

object _2673_MakeCostsOfPathsEqual:

  def minIncrements(n: Int, cost: Array[Int]): Int =
    def dfs(node: Int): (Int, Int) =
      if node * 2 > n then (0, cost(node - 1))
      else
        val (leftIncrease, leftCost)   = dfs(node * 2)
        val (rightIncrease, rightCost) = dfs(node * 2 + 1)
        val nodeIncrease               = (leftCost - rightCost).abs
        (nodeIncrease + leftIncrease + rightIncrease, leftCost.max(rightCost) + cost(node - 1))

    dfs(node = 1)._1
