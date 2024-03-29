package com.leetcode.cosminci._600

import com.leetcode.cosminci.utils

import scala.annotation.tailrec

object _524_LongestWordInDictionaryThroughDeleting:
  def main(args: Array[String]): Unit =
    println(findLongestWord("abpcplea", List("ale", "apple", "monkey", "plea")))

  def findLongestWord(s: String, dictionary: List[String]): String =
    def isSubsequence(w: String): Boolean =
      w.length == s.foldLeft(0)((i, char) => if i < w.length && w(i) == char then i + 1 else i)

    dictionary.filter(isSubsequence).minByOption(w => (-w.length, w)).getOrElse("")
