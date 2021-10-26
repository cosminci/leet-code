package io.github.cosminci.leetcode._900

import scala.collection.mutable

object _877_StoneGame:

  def main(args: Array[String]): Unit =
    println(stoneGame(Array(5, 5, 6, 5)))

  private def stoneGame(piles: Array[Int]): Boolean =
    val mem = mutable.Map.empty[(Int, Int), Int]

    def dfs(l: Int, r: Int): Int =
      mem.getOrElseUpdate(
        (l, r), {
          if l > r then 0
          else
            val aliceTurn = (r - l) % 2 == 1
            math.max(
              dfs(l + 1, r) + (if aliceTurn then piles(l) else 0),
              dfs(l, r - 1) + (if aliceTurn then piles(r) else 0)
            )
        }
      )

    dfs(l = 0, r = piles.length - 1) > piles.sum / 2
