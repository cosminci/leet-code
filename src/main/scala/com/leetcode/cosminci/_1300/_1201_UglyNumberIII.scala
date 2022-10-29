package com.leetcode.cosminci._1300

import com.leetcode.cosminci.utils.gcd

import scala.annotation.tailrec

object _1201_UglyNumberIII:
  def main(args: Array[String]): Unit =
    println(nthUglyNumber(16, 7, 1, 7))
    println(nthUglyNumber(5, 2, 2, 3))
    println(nthUglyNumber(3, 2, 3, 5))
    println(nthUglyNumber(4, 2, 3, 4))
    println(nthUglyNumber(5, 2, 11, 13))
    println(nthUglyNumber(1000000000, 2, 217983653, 336916467))

  def nthUglyNumber(n: Int, a: Int, b: Int, c: Int): Int =
    val ab  = (a.toLong * b) / gcd(a, b)
    val ac  = (a.toLong * c) / gcd(a, c)
    val bc  = (b.toLong * c) / gcd(b, c)
    val abc = (a * bc) / gcd(a, bc.toInt)

    def enoughNumbers(value: Long) =
      value / a + value / b + value / c - value / ab - value / ac - value / bc + value / abc >= n

    var (l, r) = (1L, 2 * math.pow(10, 9).toLong)
    while l < r do
      val mid = l + (r - l) / 2
      if enoughNumbers(mid) then r = mid
      else l = mid + 1
    l.toInt
