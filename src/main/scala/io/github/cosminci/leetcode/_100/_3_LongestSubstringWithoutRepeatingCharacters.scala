package io.github.cosminci.leetcode._100

import scala.collection.mutable

object _3_LongestSubstringWithoutRepeatingCharacters:
  def main(args: Array[String]): Unit =
    println(lengthOfLongestSubstring(""))

  def lengthOfLongestSubstring(s: String): Int =
    var windowStart = 0
    var maxLength   = 0
    val charIndices = mutable.Map.empty[Char, Int]

    s.indices.foreach { idx =>
      charIndices.get(s(idx)) match
        case None =>
          maxLength = math.max(maxLength, charIndices.size + 1)
        case Some(i) =>
          val charactersSkipped = s.substring(windowStart, i + 1)
          windowStart = i + 1
          charactersSkipped.foreach(charIndices.remove)
      charIndices.update(s(idx), idx)
    }

    maxLength
