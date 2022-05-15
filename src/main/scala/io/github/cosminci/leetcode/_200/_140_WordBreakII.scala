package io.github.cosminci.leetcode._200

import scala.collection.mutable

object _140_WordBreakII:

  def wordBreak(s: String, wordDict: List[String]): List[String] =
    val dict = wordDict.toSet
    val mem  = mutable.Map[Int, Seq[String]](s.length -> Seq(""))

    def dfs(start: Int): Seq[String] = mem.getOrElseUpdate(start,
      for
        end  <- start + 1 to s.length if dict.contains(s.slice(start, end))
        tail <- dfs(end)
      yield s"${s.slice(start, end)} $tail"
    )

    dfs(start = 0).map(_.trim).toList
