package com.leetcode.cosminci._900

import scala.collection.mutable

object _808_SoupServings:

  def soupServings(n: Int): Double =
    val mem = mutable.Map[(Int, Int), Double]()
    def dfs(a: Int, b: Int): Double = mem.getOrElseUpdate((a, b),
      if a <= 0 && b <= 0 then 0.5
      else if a <= 0 then 1.0
      else if b <= 0 then 0.0
      else Seq((100, 0), (75, 25), (50, 50), (25, 75))
        .map { case (da, db) => dfs(a - da, b - db) }
        .sum / 4
    )
    if (n > 5000) 1.0 else dfs(a = n, b = n)
