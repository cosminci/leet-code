package com.leetcode.cosminci._1600

import scala.collection.mutable

object _1525_NumberOfGoodWaystoSplitAString:
  def main(args: Array[String]): Unit =
    println(numSplits("abcd"))
    println(numSplits("aacaba"))
    println(numSplits("aaaaa"))

  def numSplits(s: String): Int =
    val rightCharCounts = mutable.Map.empty[Char, Int]
    val leftCharCounts  = mutable.Map.empty[Char, Int]
    s.foreach { char =>
      rightCharCounts.updateWith(char) {
        case None        => Some(1)
        case Some(count) => Some(count + 1)
      }
    }

    s.foldLeft(0) { case (ways, char) =>
      rightCharCounts.updateWith(char) {
        case Some(1) | None => None
        case Some(count)    => Some(count - 1)
      }
      leftCharCounts.updateWith(char) {
        case None        => Some(1)
        case Some(count) => Some(count + 1)
      }
      if rightCharCounts.size != leftCharCounts.size then ways else ways + 1
    }
