package io.github.cosminci.leetcode._100

import scala.collection.mutable

object _96_UniqueBSTs:
  def main(args: Array[String]): Unit =
    println(numTreesTopDownDP(19))
    println(numTreesBottomUpDP(19))

  def numTreesTopDownDP(n: Int): Int =
    val mem = mutable.Map.empty[Int, Int]

    def dfs(nodes: Int): Int =
      mem.getOrElseUpdate(
        nodes, {
          if nodes <= 1 then 1
          else
            (1 to nodes).foldLeft(0) { (count, root) =>
              count + dfs(root - 1) * dfs(nodes - root)
            }
        }
      )

    dfs(nodes = n)

  def numTreesBottomUpDP(n: Int): Int =
    val dp = Array.tabulate(n + 1)(i => if i <= 1 then 1 else 0)

    for
      depth <- 2 to n
      root  <- 1 to depth
    do dp(depth) += dp(root - 1) * dp(depth - root)

    dp.last
