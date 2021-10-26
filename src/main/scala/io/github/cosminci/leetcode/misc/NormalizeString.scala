package io.github.cosminci.leetcode.misc

import scala.collection.mutable

object NormalizeString:

  def main(args: Array[String]): Unit =
    println(normalize("  This  is   an  example string   ,   ok ?   "))

  private def normalize(str: String): String =
    val result      = mutable.Stack.empty[Char]
    val punctuation = Set('.', ',', '!', '?', ';', ':')
    str.indices.foreach { i =>
      if punctuation.contains(str(i)) then
        if result.headOption.contains(' ') then result.pop()
        result.push(str(i))
      else if str(i) == ' ' then
        if !result.headOption.contains(' ') && result.nonEmpty then result.push(str(i))
      else result.push(str(i))
    }
    if result.head == ' ' then result.pop()

    result.popAll().mkString
