package io.github.cosminci.leetcode._300

import io.github.cosminci.utils

object _242_ValidAnagram:
  def main(args: Array[String]): Unit =
    println(isAnagram("anagram", "nagaram"))

  def isAnagram(s: String, t: String): Boolean =
    if s.length != t.length then return false
    utils.characterCounts(s) == utils.characterCounts(t)
