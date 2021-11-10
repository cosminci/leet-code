package io.github.cosminci.leetcode._2100

object _2063_VowelsOfAllSubstrings:
  def countVowels(word: String): Long =
    word.indices.foldLeft(0L) { (count, i) =>
      count + Option.when("aeiou".contains(word(i)))((i + 1L) * (word.length - i)).getOrElse(0L)
    }
