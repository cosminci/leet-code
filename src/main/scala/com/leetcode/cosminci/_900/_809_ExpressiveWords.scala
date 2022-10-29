package com.leetcode.cosminci._900

object _809_ExpressiveWords {
  def main(args: Array[String]): Unit = {
    println(expressiveWords("zzzzzyyyyy", Array("zzyy", "zy", "zyy")))
    println(expressiveWords("heeellooo", Array("hello", "hi", "helo")))
  }

  def expressiveWords(s: String, words: Array[String]): Int =
    words.count(canStretch(s, _))

  def canStretch(s: String, w: String): Boolean = {
    var (wGroupStart, sGroupStart) = (0, 0)
    while (wGroupStart != w.length && sGroupStart != s.length) {
      if (w(wGroupStart) != s(sGroupStart))
        return false

      var (wGroupEnd, sGroupEnd) = (wGroupStart + 1, sGroupStart + 1)
      while (wGroupEnd < w.length && w(wGroupEnd - 1) == w(wGroupEnd))
        wGroupEnd += 1
      val wGroupLength = wGroupEnd - wGroupStart
      while (sGroupEnd < s.length && s(sGroupEnd - 1) == s(sGroupEnd))
        sGroupEnd += 1
      val sGroupLength = sGroupEnd - sGroupStart

      if (wGroupLength > sGroupLength || (wGroupLength < sGroupLength && sGroupLength < 3))
        return false

      sGroupStart = sGroupEnd
      wGroupStart = wGroupEnd
    }

    wGroupStart == w.length && sGroupStart == s.length
  }
}
