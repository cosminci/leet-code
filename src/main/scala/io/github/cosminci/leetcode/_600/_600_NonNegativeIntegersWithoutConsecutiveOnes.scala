package io.github.cosminci.leetcode._600

object _600_NonNegativeIntegersWithoutConsecutiveOnes:
  def main(args: Array[String]): Unit =
    println(findIntegers(5))

  private def findIntegers(n: Int): Int =
    if n == 0 then return 1

    def dfs(i: Int): Int =
      if i > n then return 0

      if i % 2 == 1 then 1 + dfs(i << 1)
      else 1 + dfs(i << 1) + dfs((i << 1) + 1)

    1 + dfs(1)
