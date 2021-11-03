package io.github.cosminci.leetcode._800

import scala.collection.mutable

object _712_MinAsciiDeleteSumForTwoStrings:
  def main(args: Array[String]): Unit =
    println(minimumDeleteSumTopDown("acacabcaabac", "accabaccccabaca"))
    println(minimumDeleteSumBottomUp("acacabcaabac", "accabaccccabaca"))
    println(minimumDeleteSumTopDown("sea", "eat"))
    println(minimumDeleteSumBottomUp("sea", "eat"))
    println(minimumDeleteSumTopDown("delete", "leet"))
    println(minimumDeleteSumBottomUp("delete", "leet"))

  def minimumDeleteSumTopDown(s1: String, s2: String): Int =
    val mem = mutable.Map.empty[(Int, Int), Int]

    def dfs(i: Int, j: Int): Int =
      mem.getOrElseUpdate(
        (i, j), {
          if i == s1.length && j == s2.length then 0
          else if i == s1.length then s2.substring(j).sum
          else if j == s2.length then s1.substring(i).sum
          else if s1(i) == s2(j) then dfs(i + 1, j + 1)
          else math.min(s1(i) + dfs(i + 1, j), s2(j) + dfs(i, j + 1))
        }
      )

    dfs(i = 0, j = 0)

  def minimumDeleteSumBottomUp(s1: String, s2: String): Int =
    val (m, n)      = (s1.length, s2.length)
    val s1PrefixSum = s1.scanLeft(0)(_ + _).tail
    val s2PrefixSum = s2.scanLeft(0)(_ + _).tail

    val dp = Seq.tabulate(m + 1, n + 1) { (i, j) =>
      if i > 0 && j == 0 then s1PrefixSum(i - 1)
      else if j > 0 && i == 0 then s2PrefixSum(j - 1)
      else 0
    }

    (1 to m).foldLeft(dp) { (dp, i) =>
      (1 to n).foldLeft(dp) { (dp, j) =>
        val minSum =
          if s1(i - 1) == s2(j - 1) then dp(i - 1)(j - 1)
          else math.min(s1(i - 1) + dp(i - 1)(j), s2(j - 1) + dp(i)(j - 1))
        dp.updated(i, dp(i).updated(j, minSum))
      }
    }(m)(n)
