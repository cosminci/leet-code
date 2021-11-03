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
    val builder = new StringBuilder()
    var kLeft   = k

    num.foreach { d =>
      while kLeft > 0 && builder.lastOption.exists(_ > d) do
        builder.deleteCharAt(builder.length() - 1)
        kLeft -= 1
      builder.append(d)
    }

    val result = builder.dropRight(kLeft).dropWhile(_ == '0').toString
    if result.isEmpty then "0" else result
