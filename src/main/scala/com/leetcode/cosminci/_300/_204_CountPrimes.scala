package com.leetcode.cosminci._300

object _204_CountPrimes:
  def main(args: Array[String]): Unit =
    println(countPrimes(10))
    println(countPrimes(11))

  def countPrimes(n: Int): Int =
    if n <= 1 then return 0
    val primeBitset = Array.fill(n)(1)
    primeBitset(0) = 0
    primeBitset(1) = 0

    (2 to math.sqrt(n).toInt).foreach { i =>
      if primeBitset(i) == 1 then
        var j = 2
        while i * j < n do
          primeBitset(i * j) = 0
          j += 1
    }
    primeBitset.sum
