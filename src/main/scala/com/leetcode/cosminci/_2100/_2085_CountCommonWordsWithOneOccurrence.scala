package com.leetcode.cosminci._2100

object _2085_CountCommonWordsWithOneOccurrence {
  def countWords(words1: Array[String], words2: Array[String]): Int = {
    val counts1 = words1.groupMapReduce(identity)(_ => 1)(_ + _)
    val counts2 = words2.groupMapReduce(identity)(_ => 1)(_ + _)

    counts1.count { case (w, c) => c == 1 && counts2.get(w).contains(1) }
  }
}
