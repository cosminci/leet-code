package com.leetcode.cosminci._600

import com.leetcode.cosminci.utils

object _567_PermutationInString:
  def main(args: Array[String]): Unit =
    println(checkInclusion("abc", "ccccbabbb"))

  def checkInclusion(s1: String, s2: String): Boolean =
    if s1.length > s2.length then return false

    val s1Chars = utils.characterCounts(s1)
    val s2Chars = utils.characterCounts(s2.take(s1.length))

    (s1.length until s2.length).foldLeft(s2Chars) { case (s2Chars, s2Idx) =>
      if s1Chars == s2Chars then return true
      val oldChar = s2(s2Idx - s1.length) - 'a'
      val newChar = s2(s2Idx) - 'a'
      val shrunk  = s2Chars.updated(oldChar, s2Chars(oldChar) - 1)
      shrunk.updated(newChar, shrunk(newChar) + 1)
    } == s1Chars
