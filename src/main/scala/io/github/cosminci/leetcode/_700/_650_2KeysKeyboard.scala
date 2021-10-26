package io.github.cosminci.leetcode._700

import scala.collection.mutable

object _650_2KeysKeyboard:
  private def minSteps(n: Int): Int =
    if n == 1 then 0
    else (2 to math.sqrt(n).toInt)
      .collectFirst {
        case i if n % i == 0 =>
          minSteps(n / i) + i
      }.getOrElse(n)
