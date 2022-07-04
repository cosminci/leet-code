package io.github.cosminci.leetcode._2400

import scala.collection.mutable

object _2327_NumPeopleAwareOfSecret:

  def peopleAwareOfSecret(n: Int, delay: Int, forget: Int): Int =
    val mod = 1_000_000_007
    val mem = mutable.Map.empty[Int, Long]

    def dfs(day: Int): Long = mem.getOrElseUpdate(day,
      if day <= 0 then 0
      else if day == 1 then 1
      else (day - delay until day - forget by -1).map(dfs).sum % mod
    )

    ((n until n - forget by -1).map(dfs).sum % mod).toInt
