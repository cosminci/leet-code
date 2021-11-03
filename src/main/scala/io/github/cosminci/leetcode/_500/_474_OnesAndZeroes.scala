package io.github.cosminci.leetcode._500

import scala.collection.mutable

object _474_OnesAndZeroes:
  def main(args: Array[String]): Unit =
    println(findMaxForm(Array("10", "0001", "111001", "1", "0"), 5, 3))
    println(findMaxForm(Array("10", "0", "1"), 1, 1))

  def findMaxForm(strs: Array[String], m: Int, n: Int): Int =
    def oneZeroCounts(s: String): (Int, Int) =
      s.foldLeft(0, 0) { case ((ones, zeroes), char) =>
        if char == '1' then (ones + 1, zeroes) else (ones, zeroes + 1)
      }

    val sorted = strs.map(oneZeroCounts).sortBy { case (ones, zeroes) => -(ones + zeroes) }
    val mem    = mutable.Map.empty[(Int, Int, Int), Int]
    def dfs(idx: Int, mBudget: Int, nBudget: Int): Int =
      if idx == strs.length then return 0
      if mem.contains((idx, mBudget, nBudget)) then return mem((idx, mBudget, nBudget))

      val (ones, zeroes) = sorted(idx)
      val result =
        if ones > nBudget || zeroes > mBudget then dfs(idx + 1, mBudget, nBudget)
        else
          math.max(
            1 + dfs(idx + 1, mBudget - zeroes, nBudget - ones),
            dfs(idx + 1, mBudget, nBudget)
          )

      mem.update((idx, mBudget, nBudget), result)
      result

    dfs(idx = 0, mBudget = m, nBudget = n)
