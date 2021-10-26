package io.github.cosminci.leetcode._200

object _125_ValidPalindrome:
  def isPalindrome(str: String): Boolean =
    val s = str.collect { case l if l.isLetterOrDigit => l.toLower }
    (0 until s.length / 2).forall(i => s(i) == s(s.length - 1 - i))
