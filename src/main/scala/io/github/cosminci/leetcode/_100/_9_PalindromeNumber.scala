package io.github.cosminci.leetcode._100

object _9_PalindromeNumber:
  def isPalindrome(x: Int): Boolean =
    if x < 0 || (x % 10 == 0 && x != 0) then return false

    var rev = 0
    var n   = x
    while n > rev do
      rev = rev * 10 + n % 10
      n = n / 10

    n == rev || n == rev / 10
