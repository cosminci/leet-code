package com.leetcode.cosminci._500

object _401_BinaryWatch:

  def readBinaryWatch(turnedOn: Int): Seq[String] =
    for
      hour   <- 0 until 12
      minute <- 0 until 60
      if Integer.bitCount(hour) + Integer.bitCount(minute) == turnedOn
    yield String.format("%d:%02d", hour, minute)
