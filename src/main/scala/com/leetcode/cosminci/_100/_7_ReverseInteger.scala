package com.leetcode.cosminci._100

object _7_ReverseInteger:

  def main(args: Array[String]): Unit =
    println(reverse(-2147483648))

  def reverse(n: Int): Int =
    if n == Int.MinValue then return 0
    var x = n
    var result = 0

    while math.abs(x) > 9 do
      result = result * 10 + x % 10
      x /= 10

    val absRes = math.abs(result)

    if absRes > Int.MaxValue / 10 then 0
    else if absRes == Int.MaxValue / 10 && (x > Int.MaxValue % 10 || x < Int.MinValue % 10) then 0
    else result * 10 + x
