package io.github.cosminci.leetcode._500

import scala.util.Random

object _470_ImplementRand10UsingRand7 {

  private def rand10(): Int = {
    var result = 40
    while (result >= 40)
      result = 7 * (rand7() - 1) + rand7() - 1
    result % 10 + 1
  }

  private def rand7(): Int = Random.nextInt(7) + 1
}
