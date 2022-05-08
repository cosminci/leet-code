package io.github.cosminci.leetcode._2300

import scala.collection.mutable

object _2266_CountNumOfTexts {

  def countTexts(s: String): Int = {
    val mem = mutable.Map.empty[Int, Long]
    def dfs(i: Int): Long = mem.getOrElseUpdate(i, {
      if (i == 0) 1L
      else {
        dfs(i - 1) +
          { if (i >= 2 && s(i - 1) == s(i - 2)) dfs(i - 2) else 0 } +
          { if (i >= 3 && (2 to 3).forall(j => s(i - 1) == s(i - j))) dfs(i - 3) else 0 } +
          { if (i >= 4 && "79".contains(s(i - 1)) && (2 to 4).forall(j => s(i - 1) == s(i - j))) dfs(i - 4) else 0 }
      } % 1_000_000_007
    })

    dfs(s.length).toInt
  }
}
