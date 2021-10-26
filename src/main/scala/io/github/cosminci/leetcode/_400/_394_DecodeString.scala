package io.github.cosminci.leetcode._400

import scala.collection.mutable

object _394_DecodeString:
  def main(args: Array[String]): Unit =
    println(decodeString("3[a2[c]]"))

  def decodeString(s: String): String =
    val openGroups = mutable.Stack.empty[(Int, String)]
    var idx        = 0
    while idx != s.length do
      if s(idx).isDigit then
        var end = idx + 1
        while s(end).isDigit do end += 1
        openGroups.push((s.substring(idx, end).toInt, ""))
        idx = end - 1
      else if s(idx).isLetter then
        val currentGroup = openGroups.pop()
        openGroups.push((currentGroup._1, s"${currentGroup._2}${s(idx)}"))
      else if s(idx) == ']' then
        val completeGroup = openGroups.pop()
        val result        = completeGroup._2 * completeGroup._1
        if openGroups.nonEmpty then
          val currentGroup = openGroups.pop()
          openGroups.push((currentGroup._1, s"${currentGroup._2}$result"))
        else openGroups.push((1, result))
      idx += 1
    openGroups.head._2
