package com.leetcode.cosminci._1500

import scala.collection.mutable
import scala.util.chaining.*

object _1444_NumWayOfCuttingPizza:

  def ways(pizza: Array[String], k: Int): Int =
    val mod    = 1_000_000_007
    val (m, n) = (pizza.length, pizza.head.length)

    val postSumMem = mutable.Map.empty[(Int, Int), Int]
    def postSum(r: Int, c: Int): Int = postSumMem.getOrElseUpdate((r, c),
      if r == m || c == n then 0
      else postSum(r + 1, c) + postSum(r, c + 1) - postSum(r + 1, c + 1) +
        Option.when(pizza(r)(c) == 'A')(1).getOrElse(0)
    )

    val dfsMem = mutable.Map.empty[(Int, Int, Int), Int]
    def dfs(r: Int, c: Int, k: Int): Int = dfsMem.getOrElseUpdate((r, c, k),
      if postSum(r, c) == 0 then 0
      else if k == 0 then 1
      else
        val horizontalWays = (r + 1 until m).foldLeft(0L) { (ways, nr) =>
          if postSum(r, c) - postSum(nr, c) == 0 then ways
          else (ways + dfs(nr, c, k - 1)) % mod
        }
        val verticalWays = (c + 1 until n).foldLeft(0L) { (ways, nc) =>
          if postSum(r, c) - postSum(r, nc) == 0 then ways
          else (ways + dfs(r, nc, k - 1)) % mod
        }
        ((horizontalWays + verticalWays) % mod).toInt
    )

    dfs(r = 0, c = 0, k - 1)
