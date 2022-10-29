package com.leetcode.cosminci._100

object _38_CountAndSay:

  def countAndSay(n: Int): String =
    if n == 1 then "1"
    else
      val prev = countAndSay(n - 1)
      s"${prev.tail}X"
        .foldLeft(prev.head, 1, "") { case ((prevChar, cnt, res), char) =>
          if prevChar == char then (char, cnt + 1, res)
          else (char, 1, s"$res$cnt$prevChar")
        }
        ._3
