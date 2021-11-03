package io.github.cosminci.leetcode._900

object _869_ReorderedPowerOfTwo:
  def main(args: Array[String]): Unit =
    println(reorderedPowerOf2(679213508))

  def reorderedPowerOf2(n: Int): Boolean =
    (0 until 30).exists(pow => (1 << pow).toString.sorted == n.toString.sorted)
