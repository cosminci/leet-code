package io.github.cosminci.leetcode._2300

object _2264_Largest3SameDigitNumInString:
  given Ordering[String] = (s1, s2) => s1.compareTo(s2)

  def largestGoodInteger(num: String): String =
    num.tail
      .foldLeft(num.head, 1, "") { case ((prevDigit, count, largest), digit) =>
        if prevDigit != digit then (digit, 1, largest)
        else if count == 1 then (digit, 2, largest)
        else (digit, count + 1, Array(largest, s"$digit$digit$digit").max)
      }
      ._3
