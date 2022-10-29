
package com.leetcode.cosminci._300

import com.leetcode.cosminci.utils

object _242_ValidAnagram:
  def main(args: Array[String]): Unit =
    println(isAnagram("anagram", "nagaram"))

  def isAnagram(s: String, t: String): Boolean =
    def charCounts(t: String): Map[Char, Int] = t.groupMapReduce(identity)(_ => 1)(_ + _)
    charCounts(s) == charCounts(t)
