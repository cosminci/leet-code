package io.github.cosminci.leetcode._400

object _336_PalindromePairs:

  def palindromePairs(input: Array[String]): List[List[Int]] =
    val words = input.zipWithIndex.toMap
    words.toList.flatMap { case (w, i) =>
      (w.indices :+ w.length).flatMap { j =>
        val (prefix, suffix)   = w.splitAt(j)
        val (prefixR, suffixR) = (prefix.reverse, suffix.reverse)
        val maybePair1 = Option
          .when(prefix == prefixR && suffixR != w && words.contains(suffixR))(
            List(words(suffix.reverse), i)
          )
        val maybePair2 = Option
          .when(j != w.length && suffix == suffixR && prefixR != w && words.contains(prefixR))(
            List(i, words(prefix.reverse))
          )
        List(maybePair1, maybePair2).flatten
      }
    }
