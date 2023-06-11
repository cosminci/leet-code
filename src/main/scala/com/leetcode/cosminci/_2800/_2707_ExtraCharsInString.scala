package com.leetcode.cosminci._2800

import scala.collection.mutable

object _2707_ExtraCharsInString:

  def minExtraChar(s: String, dictionary: Array[String]): Int =
    val mem = mutable.Map.empty[Int, Int]
    def dfs(i: Int): Int = mem.getOrElseUpdate(i,
      if i == s.length then 0
      else dictionary
        .collect { case w if s.startsWith(w, i) => dfs(i + w.length) }
        .minOption.getOrElse(s.length)
        .min(1 + dfs(i + 1))
    )
    dfs(i = 0)
