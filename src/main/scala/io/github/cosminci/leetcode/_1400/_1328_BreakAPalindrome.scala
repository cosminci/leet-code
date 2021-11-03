package io.github.cosminci.leetcode._1400

object _1328_BreakAPalindrome {
  def main(args: Array[String]): Unit = {
    println(breakPalindrome("abccba"))
    println(breakPalindrome("a"))
    println(breakPalindrome("aa"))
    println(breakPalindrome("aba"))
  }

  def breakPalindrome(palindrome: String): String = {
    val n = palindrome.length
    if (n == 1) return ""

    (0 until n / 2).find(idx => palindrome(idx) != 'a') match {
      case Some(idx) => palindrome.updated(idx, 'a')
      case None => palindrome.updated(n - 1, 'b')
    }
  }
}
