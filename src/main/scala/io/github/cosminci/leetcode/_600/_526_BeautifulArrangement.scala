package io.github.cosminci.leetcode._600

object _526_BeautifulArrangement:
  def main(args: Array[String]): Unit =
    println(countArrangement(10))

  def countArrangement(n: Int): Int =
    def beautiful(i: Int, j: Int) = i % j == 0 || j % i == 0

    def dfs(availableBitmask: Int, idx: Int): Int =
      if idx > n then 1
      else
        (0 until n).collect {
          case i if ((availableBitmask >> i) & 1) == 0 && beautiful(i + 1, idx) =>
            dfs(availableBitmask | (1 << i), idx + 1)
        }.sum

    dfs(availableBitmask = 0, idx = 1)
