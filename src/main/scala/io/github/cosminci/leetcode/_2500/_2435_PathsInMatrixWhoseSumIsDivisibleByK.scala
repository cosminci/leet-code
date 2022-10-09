package io.github.cosminci.leetcode._2500

object _2435_PathsInMatrixWhoseSumIsDivisibleByK:

  def numberOfPaths(grid: Array[Array[Int]], k: Int): Int =
    val (m, n) = (grid.length, grid.head.length)
    val dp     = Array.fill(m, n, k)(-1) // using mutable.Map for memoization TLEs

    def dfs(x: Int, y: Int, s: Int): Int =
      if x == m || y == n then 0
      else if x == m - 1 && y == n - 1 then if (s + grid(x)(y)) % k == 0 then 1 else 0
      else if dp(x)(y)(s) != -1 then dp(x)(y)(s)
      else
        val above = dfs(x + 1, y, (s + grid(x)(y)) % k)
        val left  = dfs(x, y + 1, (s + grid(x)(y)) % k)
        dp(x)(y)(s) = (above + left) % 1_000_000_007
        dp(x)(y)(s)

    dfs(x = 0, y = 0, s = 0)
