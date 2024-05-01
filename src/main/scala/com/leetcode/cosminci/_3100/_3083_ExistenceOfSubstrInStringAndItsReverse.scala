package com.leetcode.cosminci._3100

object _3083_ExistenceOfSubstrInStringAndItsReverse:

  def isSubstringPresent(s: String): Boolean =
    val reverse = s.reverse
    s.length > 1 && s.sliding(2).exists(reverse.contains)
