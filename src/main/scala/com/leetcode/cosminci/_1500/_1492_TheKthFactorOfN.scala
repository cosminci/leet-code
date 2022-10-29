package com.leetcode.cosminci._1500

object _1492_TheKthFactorOfN:
  def main(args: Array[String]): Unit =
    println(kthFactor(24, 6))
    println(kthFactor(7, 2))
    println(kthFactor(1000, 3))
    println(kthFactor(1, 1))
    println(kthFactor(1, 2))

  def kthFactor(n: Int, kth: Int): Int =
    var root = math.sqrt(n).toInt
    var k    = kth
    (1 to root).foreach { i =>
      if n % i == 0 then
        k -= 1; if k == 0 then return i
    }
    (root to 1 by -1).foreach { i =>
      if n % i == 0 && i * i != n then
        k -= 1; if k == 0 then return n / i
    }
    -1
