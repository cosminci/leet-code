package com.leetcode.cosminci._600

object _557_ReverseWordsInAStringIII:

  def reverseWords(s: String): String =
    s.split(' ').map(_.reverse).mkString(" ")
