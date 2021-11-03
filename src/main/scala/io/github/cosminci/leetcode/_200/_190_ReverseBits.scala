package io.github.cosminci.leetcode._200

object _190_ReverseBits:
  def main(args: Array[String]): Unit =
    println(reverseBits(1000))

  def reverseBits(x: Int): Int =
    var (result, n) = (0, x)
    (31 to 0 by -1).foreach { shift =>
      val lsb = n & 1
      result += lsb << shift
      n = n >> 1
    }
    result
