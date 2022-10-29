package com.leetcode.cosminci._100

object _3_LongestSubstringWithoutRepeatingCharacters:

  def lengthOfLongestSubstring(s: String): Int =
    s.indices
      .foldLeft(0, 0, Map.empty[Char, Int]) { case ((max, start, indices), j) =>
        indices.get(s(j)) match
          case None    => (max.max(j - start + 1), start, indices.updated(s(j), j))
          case Some(i) => (max, i + 1, indices.removedAll(s.substring(start, i + 1)).updated(s(j), j))
      }
      ._1
