package com.leetcode.cosminci._900

object _804_UniqueMorseCodeWords:

  def uniqueMorseRepresentations(words: Array[String]): Int =
    val mappings = Array(
      ".-", "-...", "-.-.", "-..", ".", "..-.", "--.", "....", "..", ".---", "-.-", ".-..", "--",
      "-.", "---", ".--.", "--.-", ".-.", "...", "-", "..-", "...-", ".--", "-..-", "-.--", "--.."
    )
    words.map(_.map(c => mappings(c - 'a')).mkString).distinct.length
