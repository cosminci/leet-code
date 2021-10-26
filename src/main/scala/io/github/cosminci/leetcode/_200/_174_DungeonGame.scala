package io.github.cosminci.leetcode._200

import scala.collection.mutable

object _174_DungeonGame:
  def main(args: Array[String]): Unit =
    val dungeon = Array(
      Array(-2, -3, 3),
      Array(-5, -10, 1),
      Array(10, 30, -5)
    )
    println(calculateMinimumHPTopDown(dungeon))
    println(calculateMinimumHPBottomUp(dungeon))

  private def calculateMinimumHPTopDown(dungeon: Array[Array[Int]]): Int =
    val (m, n) = (dungeon.length, dungeon.head.length)

    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(x: Int, y: Int): Int =
      mem.getOrElseUpdate((x, y), {
        if x == m - 1 && y == n - 1 then
          if dungeon(m - 1)(n - 1) < 0 then -dungeon(x)(y) + 1 else 1
        else
          math.max(
            1,
            if x == m - 1 then dfs(x, y + 1) - dungeon(x)(y)
            else if y == n - 1 then dfs(x + 1, y) - dungeon(x)(y)
            else math.min(dfs(x, y + 1), dfs(x + 1, y)) - dungeon(x)(y)
          )
        }
      )

    dfs(x = 0, y = 0)

  private def calculateMinimumHPBottomUp(dungeon: Array[Array[Int]]): Int =
    val (m, n) = (dungeon.length, dungeon.head.length)

    val dp = Array.fill(m + 1, n + 1)((1e9 + 5).toInt)
    dp(m)(n - 1) = 1
    dp(m - 1)(n) = 1

    for
      x <- m - 1 to 0 by -1
      y <- n - 1 to 0 by -1
    do dp(x)(y) = math.max(1, math.min(dp(x)(y + 1), dp(x + 1)(y)) - dungeon(x)(y))

    dp(0)(0)
