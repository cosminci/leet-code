package io.github.cosminci.leetcode._100

object _75_SortColors:

  def main(args: Array[String]): Unit =
    val input = Array(2, 0, 1)
    sortColorsPointers(input)
    println(input.toList)

  def sortColorsPointers(nums: Array[Int]): Unit =
    var (l, i, r) = (0, 0, nums.length - 1)
    while i <= r do
      if nums(i) == 0 then
        swap(l, i); l += 1
      else if nums(i) == 2 then
        swap(r, i); r -= 1; i -= 1
      i += 1
    def swap(i: Int, j: Int): Unit =
      val tmp = nums(i)
      nums(i) = nums(j)
      nums(j) = tmp

  def sortColorsCounts(nums: Array[Int]): Unit =
    val (zeroCount, oneCount, twoCount) = nums.foldLeft((0, 0, 0)) { case ((c0, c1, c2), n) =>
      if n == 0 then (c0 + 1, c1, c2)
      else if n == 1 then (c0, c1 + 1, c2)
      else (c0, c1, c2 + 1)
    }
    (0 until zeroCount).foreach(i => nums(i) = 0)
    (zeroCount until zeroCount + oneCount).foreach(i => nums(i) = 1)
    (zeroCount + oneCount until zeroCount + oneCount + twoCount).foreach(i => nums(i) = 2)
