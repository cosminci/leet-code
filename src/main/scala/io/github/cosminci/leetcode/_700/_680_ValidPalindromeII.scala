package io.github.cosminci.leetcode._700

object _680_ValidPalindromeII:
  def validPalindrome(s: String): Boolean =
    def dfs(l: Int, r: Int, removed: Boolean): Boolean =
      if l >= r then true
      else if s(l) != s(r) && removed then false
      else if s(l) != s(r) then dfs(l + 1, r, removed = true) || dfs(l, r - 1, removed = true)
      else dfs(l + 1, r - 1, removed)

    dfs(l = 0, r = s.length - 1, removed = false)
