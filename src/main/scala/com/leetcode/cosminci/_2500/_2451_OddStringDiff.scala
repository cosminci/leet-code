package com.leetcode.cosminci._2500

object _2451_OddStringDiff:

  def oddString(words: Array[String]): String =
    def dfs(j: Int): String =
      words.indices
        .groupBy(i => words(i)(j + 1) - words(i)(j))
        .collectFirst { case (_, Seq(i)) => words(i) }
        .getOrElse(dfs(j + 1))

    dfs(j = 0)
