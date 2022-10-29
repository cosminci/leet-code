package com.leetcode.cosminci._2300

object _2273_FindResultantArrayAfterRemovingAnagrams:

  def removeAnagrams(words: Array[String]): List[String] =
    words.indices.collect {
      case i if i == 0 || words(i).sorted != words(i - 1).sorted => words(i)
    }.toList
