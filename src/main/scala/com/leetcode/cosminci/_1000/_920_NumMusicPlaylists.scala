package com.leetcode.cosminci._1000

import scala.collection.mutable

object _920_NumMusicPlaylists:

  def numMusicPlaylists(n: Int, goal: Int, k: Int): Int =
    val mem = mutable.Map[(Int, Int), Long]()
    def dfs(n: Int, goal: Int): Long = mem.getOrElseUpdate((n, goal),
      if n == 0 && goal == 0 then 1
      else if n == 0 || goal == 0 then 0
      else
        val pick = dfs(n - 1, goal - 1) * n
        val skip = dfs(n, goal - 1) * (n - k).max(0)
        (pick + skip) % 1_000_000_007
    )
    dfs(n, goal).toInt
