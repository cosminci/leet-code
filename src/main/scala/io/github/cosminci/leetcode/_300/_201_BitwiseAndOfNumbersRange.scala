package io.github.cosminci.leetcode._300

object _201_BitwiseAndOfNumbersRange:
  def main(args: Array[String]): Unit =
    println(rangeBitwiseAnd(5, 7))
    println(rangeBitwiseAnd(1, Int.MaxValue))

  def rangeBitwiseAnd(left: Int, right: Int): Int =
    var (l, r) = (left, right)
    var shifts = 0

    while l != r do
      shifts += 1
      l = l >> 1
      r = r >> 1

    l << shifts
