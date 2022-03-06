package io.github.cosminci.leetcode._2200

object _2191_SortJumbledNums:
  def sortJumbled(mapping: Array[Int], nums: Array[Int]): Array[Int] =
    @annotation.tailrec
    def jumble(n: Int, res: Int, pow10: Int): Int =
      if n == 0 then res
      else jumble(n / 10, res + mapping(n % 10) * pow10, pow10 * 10)

    nums.sortBy { n =>
      if n < 10 then mapping(n) else jumble(n, res = 0, pow10 = 1)
    }
