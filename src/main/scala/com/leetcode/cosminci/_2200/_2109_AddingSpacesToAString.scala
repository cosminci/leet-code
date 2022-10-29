package com.leetcode.cosminci._2200

object _2109_AddingSpacesToAString:
  def main(args: Array[String]): Unit =
    println(addSpaces("spacing", Array(0, 3, 4, 7)))

  def addSpaces(s: String, spaces: Array[Int]): String =
    val result = new StringBuilder(s.slice(0, spaces.head))
    (spaces.tail :+ s.length).foldLeft(spaces.head) { case (prevIdx, currIdx) =>
      result.append(" ").append(s.slice(prevIdx, currIdx))
      currIdx
    }
    result.toString()
