package com.leetcode.cosminci._400

object _343_IntegerBreak:
  def main(args: Array[String]): Unit =
    println(integerBreak(10))
    println(integerBreak(2))

  def integerBreak(n: Int): Int =
    def product(parts: Int): Int =
      (0 until parts).foldLeft(1) { case (product, partIdx) =>
        product * (n / parts + (if n % parts > partIdx then 1 else 0))
      }

    (2 to n / 2 + 1).map(product).max
