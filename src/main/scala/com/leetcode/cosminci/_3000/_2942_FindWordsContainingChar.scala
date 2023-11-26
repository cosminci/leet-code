package com.leetcode.cosminci._3000

object _2942_FindWordsContainingChar:

  def findWordsContaining(words: Array[String], x: Char): List[Int] =
    words.indices.filter(i => words(i).contains(x))
