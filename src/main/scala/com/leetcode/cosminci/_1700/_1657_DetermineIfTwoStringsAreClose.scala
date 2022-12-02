package com.leetcode.cosminci._1700

object _1657_DetermineIfTwoStringsAreClose:

  def closeStrings(word1: String, word2: String): Boolean =
    def freq(w: String) = w.groupMapReduce(identity)(_ => 1)(_ + _)

    val (freq1, freq2) = (freq(word1), freq(word2))
    freq1.toSeq.sorted == freq2.values.toSeq.sorted && freq1.keySet == freq2.keySet
