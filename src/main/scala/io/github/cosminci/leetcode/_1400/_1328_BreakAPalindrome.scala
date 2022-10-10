package io.github.cosminci.leetcode._1400

object _1328_BreakAPalindrome:

  def breakPalindrome(palindrome: String): String =
    if palindrome.length == 1 then ""
    else
      (0 until palindrome.length / 2).find(palindrome(_) != 'a') match
        case Some(idx) => palindrome.updated(idx, 'a')
        case None      => palindrome.updated(palindrome.length - 1, 'b')
