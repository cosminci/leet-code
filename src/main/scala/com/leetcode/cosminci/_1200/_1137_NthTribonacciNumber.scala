package com.leetcode.cosminci._1200

object _1137_NthTribonacciNumber:
  def main(args: Array[String]): Unit =
    println(tribonacci(4))
    println(tribonacci(25))

  def tribonacci(n: Int): Int =
    if n == 0 then 0 else (3 to n).foldLeft(Seq(0, 1, 1))((prev, _) => prev.tail :+ prev.sum).last
