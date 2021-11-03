package io.github.cosminci.leetcode._600

import io.github.cosminci.utils

object _567_PermutationInString:
  def main(args: Array[String]): Unit =
    println(checkInclusion("adc", "dcda"))

  def checkInclusion(s1: String, s2: String): Boolean =
    if s1.length > s2.length then return false

    val s1Chars = utils.characterCounts(s1)
    val s2Chars = utils.characterCounts(s2.take(s1.length)).toArray

    var s2Idx = s1.length
    while s2Idx != s2.length && !s1Chars.sameElements(s2Chars) do
      s2Chars(s2(s2Idx - s1.length) - 'a') -= 1
      s2Chars(s2(s2Idx) - 'a') += 1
      s2Idx += 1
    s1Chars.sameElements(s2Chars)
