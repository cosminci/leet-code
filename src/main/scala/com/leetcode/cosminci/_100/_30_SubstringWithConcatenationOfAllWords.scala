package com.leetcode.cosminci._100

object _30_SubstringWithConcatenationOfAllWords:

  def findSubstring(s: String, words: Array[String]): List[Int] =
    val wordBag  = words.groupBy(identity).view.mapValues(_.length).toMap
    val wordLen  = words.head.length
    val totalLen = wordLen * words.length

    def updateNeed(r: Int, need: Map[String, Int]) =
      need.updatedWith(s.slice(r, r + wordLen)) {
        case None | Some(1) => None
        case Some(c)        => Some(c - 1)
      }

    (0 until s.length - totalLen + 1).flatMap { l =>
      val matched = Iterator
        .iterate((l, wordBag)) { case (r, need) => (r + wordLen, updateNeed(r, need)) }
        .dropWhile { case (r, need) => r < l + totalLen && need.contains(s.slice(r, r + wordLen)) }
        .next()._2.isEmpty
      Option.when(matched)(l)
    }.toList
