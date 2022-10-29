package com.leetcode.cosminci._900

object _848_ShiftingLevers:
  def main(args: Array[String]): Unit =
    println(shiftingLetters("abc", Array(3, 5, 9)))

  def shiftingLetters(s: String, shifts: Array[Int]): String =
    val result     = new StringBuilder()
    var shiftCount = 0

    (s.length - 1 to 0 by -1).foreach { i =>
      shiftCount = (shiftCount + shifts(i))    % 26
      result.append(((s(i) - 'a' + shiftCount) % 26 + 'a').toChar)
    }

    result.reverse.toString
