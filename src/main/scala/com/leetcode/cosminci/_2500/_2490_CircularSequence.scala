package com.leetcode.cosminci._2500

object _2490_CircularSequence:

  def isCircularSentence(sentence: String): Boolean =
    val words = sentence.split(' ')
    (words :+ words.head)
      .sliding(2)
      .forall(pair => pair.head.last == pair.last.head)
