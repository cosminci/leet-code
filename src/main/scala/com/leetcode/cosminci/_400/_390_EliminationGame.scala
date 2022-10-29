package com.leetcode.cosminci._400

object _390_EliminationGame:
  def main(args: Array[String]): Unit =
    println(lastRemaining(100))

  def lastRemaining(num: Int): Int =
    def leftToRight(n: Int): Int =
      if n <= 2 then n else 2 * rightToLeft(n / 2)

    def rightToLeft(n: Int): Int =
      if n <= 2 then 1
      else if n % 2 == 1 then 2 * leftToRight(n / 2)
      else 2 * leftToRight(n / 2) - 1

    leftToRight(num)
