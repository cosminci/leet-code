package io.github.cosminci.leetcode._700

import scala.collection.mutable

object _646_MaxLengthOfPairChain:
  def main(args: Array[String]): Unit =
    println(findLongestChainTopDown(Array(Array(1, 2), Array(2, 3), Array(3, 4))))
    println(findLongestChainBottomUp(Array(Array(1, 2), Array(2, 3), Array(3, 4))))
    println(findLongestChainGreedy(Array(Array(1, 2), Array(2, 3), Array(3, 4))))

  def findLongestChainTopDown(pairs: Array[Array[Int]]): Int =
    pairs.sortInPlaceBy(_.head)

    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(idx: Int, minStart: Int): Int =
      mem.getOrElseUpdate(
        (idx, minStart), {
          if idx == pairs.length then 0
          else if pairs(idx).head < minStart then dfs(idx + 1, minStart)
          else math.max(dfs(idx + 1, minStart), 1 + dfs(idx + 1, pairs(idx).last + 1))
        }
      )

    dfs(idx = 0, minStart = -1000)

  def findLongestChainBottomUp(pairs: Array[Array[Int]]): Int =
    pairs.sortInPlaceBy(_.head)

    val dp = Array.fill(pairs.length)(1)
    for
      next <- 1 until pairs.length
      prev <- 0 until next
      if (pairs(prev).last < pairs(next).head)
    do dp(next) = math.max(dp(next), dp(prev) + 1)

    dp.max

  def findLongestChainGreedy(pairs: Array[Array[Int]]): Int =
    pairs
      .sortBy(_.last)
      .foldLeft(0, Int.MinValue) { case ((longest, currEnd), Array(start, end)) =>
        if currEnd < start then (longest + 1, end)
        else (longest, currEnd)
      }
      ._1
