package io.github.cosminci.leetcode._700

import scala.collection.mutable

object _678_ValidParanthesisString:
  def main(args: Array[String]): Unit =
    println(checkValidString("((()**()**"))

  def checkValidString(s: String): Boolean =
    s.foldLeft(0, 0) { case ((balanceMin, balanceMax), char) =>
      char match
        case '(' =>
          (balanceMin + 1, balanceMax + 1)
        case ')' =>
          if balanceMax == 0 then return false
          (math.max(balanceMin - 1, 0), balanceMax - 1)
        case _ =>
          (math.max(balanceMin - 1, 0), balanceMax + 1)
    }._1 == 0
