package io.github.cosminci.leetcode._2300

import scala.collection.mutable

object _2209_MinWhiteTilesAfterCoveringWithCarpets:
  def minimumWhiteTiles(floor: String, carpets: Int, len: Int): Int =
    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(i: Int, left: Int): Int =
      mem.getOrElseUpdate((i, left), {
        if left < 0 then Int.MaxValue
        else if i < 0 then 0
        else dfs(i - len, left - 1).min(dfs(i - 1, left) + Option.when(floor(i) == '1')(1).getOrElse(0))
      })

    dfs(i = floor.indices.last, left = carpets)
