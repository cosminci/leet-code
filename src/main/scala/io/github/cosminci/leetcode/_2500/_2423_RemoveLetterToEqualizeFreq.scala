package io.github.cosminci.leetcode._2500

object _2423_RemoveLetterToEqualizeFreq:

  def equalFrequency(word: String): Boolean =
    word.indices.exists { i =>
      val without = word.take(i) ++ word.substring(i + 1)
      without.groupMapReduce(identity)(_ => 1)(_ + _).values.toSet.size == 1
    }
