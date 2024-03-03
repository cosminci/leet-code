package com.leetcode.cosminci._3100

import scala.util.chaining.*

object _3035_MaxPalindromesAfterOps:

  def maxPalindromesAfterOperations(words: Array[String]): Int =
    words.map(_.length).sorted
      .foldLeft(0, words.mkString.groupMapReduce(identity)(_ => 1)(_ + _).values.map(_ / 2).sum) {
        case ((res, pairs), len) =>
          (if (pairs - len / 2) >= 0 then res + 1 else res, pairs - len / 2)
      }.pipe { case (res, _) => res }
