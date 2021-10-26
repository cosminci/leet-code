package io.github.cosminci.leetcode._200

object _191_NumberOfOneBits:
  def main(args: Array[String]): Unit =
    println(hammingWeight(-3))

  private def hammingWeight(n: Int): Int =
    (0 to 31).foldLeft(0)((count, shift) => count + (n >> shift & 1))

