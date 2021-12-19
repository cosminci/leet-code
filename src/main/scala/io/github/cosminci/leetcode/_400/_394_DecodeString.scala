package io.github.cosminci.leetcode._400

import scala.annotation.tailrec

object _394_DecodeString:
  def main(args: Array[String]): Unit =
    println(decodeString("2[abc]3[cd]ef"))
    println(decodeString("abc3[cd]xyz"))

  def decodeString(s: String): String =
    def destructure(stack: Seq[(Int, String)]) =
      Option.when(stack.nonEmpty)(stack.dropRight(1), stack.last).getOrElse(Seq.empty, (1, ""))

    @annotation.tailrec
    def dfs(idx: Int, stack: Seq[(Int, String)]): String =
      if idx == s.length then stack.head._2
      else if s(idx).isDigit then
        val end = s.indexWhere(char => !char.isDigit, from = idx)
        dfs(end, stack :+ (s.substring(idx, end).toInt, ""))
      else if s(idx).isLetter then
        val end = Some(s.indexWhere(char => !char.isLetter, from = idx)).filter(_ >= 0).getOrElse(s.length)
        val (rest, (count, letters)) = destructure(stack)
        dfs(end, rest :+ (count, s"$letters${s.substring(idx, end)}"))
      else if s(idx) == ']' then
        val (rest, (count, letters))             = destructure(stack)
        val (prevRest, (prevCount, prevLetters)) = destructure(rest)
        dfs(idx + 1, prevRest :+ (prevCount, s"$prevLetters${letters * count}"))
      else dfs(idx + 1, stack)

    dfs(idx = 0, stack = Seq.empty)
