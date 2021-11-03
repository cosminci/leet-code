package io.github.cosminci.leetcode._200

object _151_ReverseWordsInAString:
  def main(args: Array[String]): Unit =
    println(reverseWords("the sky is blue"))
    println(reverseWords("  hello  world  "))

  def reverseWordsStd(str: String): String =
    str.split(" ").filter(_.trim.size > 0).reverse.mkString(" ")

  def reverseWords(str: String): String =
    val s = new StringBuilder(str)
    reverse(s, 0, s.length)

    var (leftStart, leftEnd) = (0, 0)
    while leftStart < s.length do
      while leftStart < s.length && s(leftStart) == ' ' do leftStart += 1

      leftEnd = leftStart
      while leftEnd < s.length && s(leftEnd) != ' ' do leftEnd += 1

      if leftEnd <= s.length then reverse(s, leftStart, leftEnd)
      leftStart = leftEnd

    cleanupSpaces(s)
    s.toString()

  def cleanupSpaces(s: StringBuilder) =
    var i = 0
    while i < s.length() do
      val prev = if i > 0 then s(i - 1) else ' '
      val next = if i < s.length() - 1 then s(i + 1) else ' '
      if s(i) == ' ' && (prev == ' ' || next == ' ') then s.deleteCharAt(i)
      else i += 1

  def reverse[T](s: StringBuilder, start: Int, end: Int) =
    val last = start + (end - start) / 2
    (0 until last - start).foreach { i =>
      val tmp = s(start + i)
      s(start + i) = s(end - 1 - i)
      s(end - 1 - i) = tmp
    }
