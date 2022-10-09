package io.github.cosminci.leetcode._2500

object _2434_UsingRobotToPrintLexicoSmallestString:

  def robotWithString(s: String): String =
    // lots of strings - switch t and paper type to mutable.StringBuilder() to pass
    s.foldLeft(s.groupMapReduce(identity)(_ => 1)(_ + _), 'a', "", "") { case ((freq, low, t, paper), ch) =>
      val newFreq = freq.updated(ch, freq(ch) - 1)
      val newLow = Iterator
        .iterate(low)(low => (low + 1).toChar)
        .dropWhile(low => low <= 'z' && newFreq.getOrElse(low, 0) == 0)
        .next()
      val (newT, newPaper) = Iterator
        .iterate((t :+ ch, paper)) { case (t, paper) => (t.dropRight(1), paper :+ t.last) }
        .dropWhile { case (t, paper) => t.lastOption.exists(_ <= newLow) }
        .next()
      (newFreq, newLow, newT, newPaper)
    }._4
