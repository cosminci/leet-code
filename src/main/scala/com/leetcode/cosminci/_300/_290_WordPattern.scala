package com.leetcode.cosminci._300

object _290_WordPattern:
  def wordPattern(pattern: String, s: String): Boolean =
    val words = s.split(' ')
    if words.length != pattern.length then return false

    words
      .zip(pattern)
      .foldLeft(Map.empty[String, Char], Map.empty[Char, String]) { case ((s2c, c2s), (word, char)) =>
        (s2c.get(word), c2s.get(char)) match
          case (Some(_), None)    => return false
          case (None, Some(_))    => return false
          case (None, None)       => (s2c.updated(word, char), c2s.updated(char, word))
          case (Some(c), Some(w)) => if word == w && char == c then (s2c, c2s) else return false
      }
      ._1.size >= 0
