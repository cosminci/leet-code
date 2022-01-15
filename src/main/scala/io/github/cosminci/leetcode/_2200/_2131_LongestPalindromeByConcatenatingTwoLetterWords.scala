package io.github.cosminci.leetcode._2200

object _2131_LongestPalindromeByConcatenatingTwoLetterWords:
  def longestPalindrome(words: Array[String]): Int =
    val (_, pairs, sym) = words.foldLeft(Map.empty[String, Int].withDefaultValue(0), 0, 0) {
      case ((counter, pairs, sym), word) =>
        val rev   = word.reverse
        val isSym = word == rev
        if counter(rev) > 0 then
          (counter.updated(rev, counter(rev) - 1), pairs + 1, Option.when(isSym)(sym - 1).getOrElse(sym))
        else
          (counter.updated(word, counter(word) + 1), pairs, Option.when(isSym)(sym + 1).getOrElse(sym))
    }
    4 * pairs + Option.when(sym > 0)(2).getOrElse(0)
