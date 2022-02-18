package io.github.cosminci.leetcode._500

import scala.collection.mutable

object _402_RemoveKDigits:
  def main(args: Array[String]): Unit =
    println(removeKdigits("1432219", 3))
    println(removeKdigits("10200", 1))
    println(removeKdigits("10", 3))
    println(removeKdigits("1234567890", 9))
    println(removeKdigits("9", 1))

  def removeKdigits(num: String, k: Int): String =
    @annotation.tailrec
    def dfs(stack: Seq[Char], idx: Int, kLeft: Int): Seq[Char] =
      if idx == num.length then stack.dropRight(kLeft)
      else if kLeft == 0 then stack ++ num.slice(idx, num.length)
      else
        stack.lastOption match
          case None                    => dfs(stack :+ num(idx), idx + 1, kLeft)
          case Some(v) if v > num(idx) => dfs(stack.dropRight(1), idx, kLeft - 1)
          case Some(v)                 => dfs(stack :+ num(idx), idx + 1, kLeft)

    val result = dfs(stack = Seq.empty, idx = 0, kLeft = k).dropWhile(_ == '0')
    Option.when(result.isEmpty)("0").getOrElse(result.mkString)
