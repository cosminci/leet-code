package com.leetcode.cosminci._500

import scala.util.chaining.*

object _405_ConvertNumToHex:

  def toHex(num: Int): String =
    if num == 0 then "0"
    else Iterator
      .iterate((num, "")) { case (n, hex) => (n >>> 4, s"${"0123456789abcdef" (n & 15)}$hex") }
      .dropWhile { case (n, _) => n != 0 }.next()
      .pipe { case (_, hex) => hex }
