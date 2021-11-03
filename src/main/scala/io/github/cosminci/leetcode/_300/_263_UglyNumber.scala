package io.github.cosminci.leetcode._300

object _263_UglyNumber:
  def main(args: Array[String]): Unit =
    println(isUgly(6))

  def isUgly(n: Int): Boolean =
    var num = n
    Seq(2, 3, 5).foreach { divisor =>
      while num != 0 && num % divisor == 0 do num = num / divisor
    }
    num == 1
