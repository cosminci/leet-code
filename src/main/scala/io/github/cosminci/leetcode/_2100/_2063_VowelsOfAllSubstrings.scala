package io.github.cosminci.leetcode._2100

object _2063_VowelsOfAllSubstrings:
  def countVowels(word: String): Long =
    word.indices.foldLeft(0L) { (count, i) =>
      count + Option.when("aeiou".contains(word(i)))((i + 1L) * (word.length - i)).getOrElse(0L)
    }

  def countVowels2(word: String): Long =
    word.indices.flatMap(i => Option.when("aeiou".contains(word(i)))((i + 1L) * (word.length - i))).sum

  def countVowels3(word: String): Long =
    word.indices.filter(i => "aeiou".contains(word(i))).map(i => (i + 1L) * (word.length - i)).sum

  def countVowels4(word: String): Long =
    word.indices.collect { case i if "aeiou".contains(word(i)) =>
      (i + 1L) * (word.length - i)
    }.sum