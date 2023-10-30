package com.leetcode.cosminci._3000

import scala.util.chaining.*

object _2904_ShortestAndLexicoSmallestBeautifulStr:

  def shortestBeautifulSubstring(s: String, k: Int): String =
    def dfs(l: Int, r: Int, k: Int): Option[String] =
      if r == s.length then None
      else
        (if s(r) == '1' then k - 1 else k).pipe { k =>
          Iterator
            .iterate((l, k)) { case (l, k) => if s(l) == '1' then (l + 1, k + 1) else (l + 1, k) }
            .dropWhile { case (l, k) => k < 0 || l < r && s(l) == '0' }.next()
            .pipe { case (l, k) =>
              val res = Option.when(k == 0)(s.substring(l, r + 1))
              Seq(res, dfs(l, r + 1, k)).flatten.minByOption(s => (s.length, s))
            }
        }

    dfs(l = 0, r = 0, k).getOrElse("")
