package io.github.cosminci.leetcode._900

import scala.collection.mutable

object _844_BackspaceStringCompare:
  def main(args: Array[String]): Unit =
    println(backspaceCompare("y#fo##f", "y#f#o##f"))

  def backspaceCompare(s: String, t: String): Boolean =
    val stack = mutable.Stack.empty[Char]

    def getTyped(str: String): String =
      str.foreach { c =>
        if c == '#' then stack.headOption.foreach(_ => stack.pop()) else stack.push(c)
      }
      stack.popAll().mkString

    getTyped(s) == getTyped(t)
