package com.leetcode.cosminci._2800

import scala.util.chaining._

object _2744_FindMaxStringPairs:

  def maximumNumberOfStringPairs(words: Array[String]): Int =
    words
      .foldLeft(0, words.toSet) { case ((res, set), w) =>
        val score = if ((set - w).contains(w.reverse)) 1 else 0
        (res + score, set - w)
      }.pipe { case (res, _) => res }
