package io.github.cosminci.leetcode._2200

object _2119_SameNumberAfterDoubleReversal:
  def main(args: Array[String]): Unit =
    println(isSameAfterReversals(526))

  def isSameAfterReversals(num: Int): Boolean =
    @annotation.tailrec
    def dfs(n: Int, r: Int): Int =
      if n == 0 then r
      else dfs(n / 10, r * 10 + n % 10)

    num == dfs(dfs(num, 0), 0)
