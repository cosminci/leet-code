package io.github.cosminci.leetcode._600

object _509_FibonacciNumber:

  def main(args: Array[String]): Unit =
    println(fib(4))

  private def fib(n: Int): Int =
    var prev2 = 0
    var prev1 = 1
    if n == 0 then return prev2
    if n == 1 then return prev1
    (2 to n).foreach { _ =>
      val f = prev2 + prev1
      prev2 = prev1
      prev1 = f
    }
    prev1
