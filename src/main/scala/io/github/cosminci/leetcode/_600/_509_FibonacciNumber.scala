package io.github.cosminci.leetcode._600

object _509_FibonacciNumber:

  def main(args: Array[String]): Unit =
    println(fib(4))

  def fib(n: Int): Int =
    Iterator
      .iterate((0, 1)) { case (prev2, prev1) => (prev1, prev2 + prev1) }
      .drop(n + 1)
      .next()
      ._1
