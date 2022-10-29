package com.leetcode.cosminci._100

object _29_IntegerDivision:
  def main(args: Array[String]): Unit =
    println(divide(-2147483648, -1))
    println(divide(18, -3))

  def divide(dividend: Int, divisor: Int): Int =
    if dividend == Int.MinValue && divisor == -1 then return Int.MaxValue

    var (a, b, res) = (math.abs(dividend), math.abs(divisor), 0)
    (31 to 0 by -1).foreach { x =>
      if (a >> x) - b >= 0 then
        res += 1 << x
        a -= (b << x)
    }

    if dividend > 0 ^ divisor > 0 then -res else res
