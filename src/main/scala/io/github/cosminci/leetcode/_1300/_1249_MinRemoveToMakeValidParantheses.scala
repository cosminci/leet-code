package io.github.cosminci.leetcode._1300

import scala.collection.mutable

object _1249_MinRemoveToMakeValidParantheses:
  def main(args: Array[String]): Unit =
    println(minRemoveToMakeValid("lee(t(c)o)de)"))
    println(minRemoveToMakeValid("a)b(c)d"))
    println(minRemoveToMakeValid("))(("))
    println(minRemoveToMakeValid("(a(b(c)d)"))

  def minRemoveToMakeValid(s: String): String =
    val paranIndices = mutable.Stack.empty[(Char, Int)]

    s.indices.foreach { i =>
      if s(i) == ')' then
        if paranIndices.headOption.exists(_._1 == '(') then paranIndices.pop()
        else paranIndices.push((')', i))
      else if s(i) == '(' then paranIndices.push(('(', i))
    }

    val toRemove = paranIndices.map(_._2)
    s.indices.filterNot(toRemove.contains).map(s).mkString
