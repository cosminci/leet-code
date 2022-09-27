package io.github.cosminci.leetcode._900

import scala.collection.mutable

object _838_PushDominoes:

  def pushDominoes(dominoes: String): String =
    val padded = 'L' +: dominoes :+ 'R'

    val result = new mutable.StringBuilder()
    (1 until padded.length).foldLeft(0) { (left, right) =>
      if padded(right) == '.' then left
      else
        val dist = right - left - 1
        if padded(right) == padded(left) then
          result.appendAll(padded(right).toString * dist)
        else if padded(left) == 'R' && padded(right) == 'L' then
          val (quot, rem) = (dist / 2, dist % 2)
          result.appendAll("R" * quot ++ "." * rem ++ "L" * quot)
        else
          result.appendAll("." * dist)

        result.append(padded(right))
        right
    }
    result.dropRight(1).mkString
