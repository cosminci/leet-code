package io.github.cosminci.leetcode._400

object _324_WiggleSortII:
  def main(args: Array[String]): Unit =
    val input = Array(1, 3, 2, 2, 3, 1)
    wiggleSort(input)
    print(input.toList)

  def wiggleSort(nums: Array[Int]): Unit =
    val sorted = nums.sorted
    val n      = nums.length
    var (l, r) = ((n + 1) / 2 - 1, n - 1)
    nums.indices.foreach { i =>
      if i % 2 == 0 then
        nums(i) = sorted(l)
        l -= 1
      else
        nums(i) = sorted(r)
        r -= 1
    }
