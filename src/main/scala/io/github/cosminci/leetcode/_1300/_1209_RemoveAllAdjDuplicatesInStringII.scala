package io.github.cosminci.leetcode._1300

object _1209_RemoveAllAdjDuplicatesInStringII:

  def removeDuplicates(s: String, k: Int): String =
    s.foldLeft(Array.empty[(Char, Int)]) { case (stack, char) =>
      stack.lastOption match
        case Some((prevChar, count)) if prevChar == char =>
          if count == k - 1 then stack.dropRight(1)
          else stack.dropRight(1) :+ (char, count + 1)
        case _ =>
          stack :+ (char, 1)
    }.flatMap { case (char, count) => Array.fill(count)(char) }
      .mkString
