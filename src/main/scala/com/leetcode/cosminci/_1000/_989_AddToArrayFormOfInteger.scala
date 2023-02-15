package com.leetcode.cosminci._1000

import scala.util.chaining.*

object _989_AddToArrayFormOfInteger:

  def addToArrayForm(num: Array[Int], k: Int): List[Int] =
    val (result, carry) = num.foldRight((List.empty[Int], k)) {
      case (value, (result, carry)) =>
        ((value + carry) % 10 +: result, (value + carry) / 10)
    }
    Iterator
      .iterate((result, carry)) { case (result, carry) => ((carry % 10) +: result, carry / 10) }
      .dropWhile { case (_, carry) => carry > 0 }.next()
      .pipe { case (result, _) => result }
