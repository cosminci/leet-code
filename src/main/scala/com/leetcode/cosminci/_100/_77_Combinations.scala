package com.leetcode.cosminci._100

object _77_Combinations:
  def main(args: Array[String]): Unit =
    println(combine(5, 3))
    println(combine(13, 13))

  def combine(inputN: Int, inputK: Int): List[List[Int]] =
    def dfs(n: Int, k: Int): Seq[Seq[Int]] =
      if k == 1 then return (1 to n).map(i => Seq(i))
      (n to k by -1).flatMap { head =>
        combine(head - 1, k - 1).map(_.appended(head))
      }
    dfs(inputN, inputK).map(_.toList).toList
