package com.leetcode.cosminci._2400

object _2380_TimeNeededToRearrangeABinaryString:

  def secondsToRemoveOccurrences(s: String): Int =
    Iterator
      .iterate(s)(_.replace("01", "10"))
      .takeWhile(s => !s.startsWith(s.filter(_ == '1')))
      .length
