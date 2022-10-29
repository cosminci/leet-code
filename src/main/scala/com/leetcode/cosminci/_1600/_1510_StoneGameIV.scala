package com.leetcode.cosminci._1600

import scala.collection.mutable

object _1510_StoneGameIV:
  def main(args: Array[String]): Unit =
    println(winnerSquareGame(92719))

  def winnerSquareGame(n: Int): Boolean =
    val mem = mutable.Map.empty[Int, Boolean]
    def dfs(n: Int): Boolean = mem.getOrElseUpdate(n,
      n > 0 && Iterator
        .iterate(math.sqrt(n).toInt)(i => i - 1)
        .takeWhile(_ > 0)
        .exists(v => !dfs(n - v * v))
    )
    dfs(n)
