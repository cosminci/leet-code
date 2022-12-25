package com.leetcode.cosminci._2600

object _2511_MaxFortsThatCanBeCaptured:

  def captureForts(forts: Array[Int]): Int =
    forts.foldLeft(2, 0, 0) { case ((prev, len, prevMax), curr) =>
      if curr == 0 then (prev, len + 1, prevMax)
      else if prev != curr && prev != 2 then (curr, 0, prevMax.max(len))
      else (curr, 0, prevMax)
    }._3
