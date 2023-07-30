package com.leetcode.cosminci._100

import com.leetcode.cosminci.utils.decrementCounter

object _30_SubstringWithConcatenationOfAllWords:

  def findSubstring(s: String, words: Array[String]): List[Int] =
    val wordBag  = words.groupBy(identity).view.mapValues(_.length).toMap
    val wordLen  = words.head.length
    val totalLen = wordLen * words.length

    (0 until s.length - totalLen + 1).flatMap { l =>
      val matched = Iterator
        .iterate((l, wordBag)) { case (r, need) => (r + wordLen, decrementCounter(need, s.slice(r, r + wordLen))) }
        .dropWhile { case (r, need) => r < l + totalLen && need.contains(s.slice(r, r + wordLen)) }
        .next()._2.isEmpty
      Option.when(matched)(l)
    }.toList
