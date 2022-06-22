package io.github.cosminci.leetcode._2400

object _2309_GreatestEnglishLetterInUpperAndLowerCase:

  def greatestLetter(s: String): String =
    ('Z'.toInt to 'A'.toInt by -1)
      .map(_.toChar)
      .find(upper => s.contains(upper) && s.contains(upper.toLower))
      .map(_.toString)
      .getOrElse("")
