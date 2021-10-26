package io.github.cosminci.leetcode._100

object _31_NextPermutation:

  def main(args: Array[String]): Unit =
    val arr = Array(1, 5, 8, 4, 7, 6, 5, 3, 1)
    nextPermutation(arr)
    println(arr.toList)

  private def nextPermutation(nums: Array[Int]): Unit =
    if nums.length < 2 then return

    var i = nums.length - 2
    while i >= 0 && nums(i) >= nums(i + 1) do i -= 1

    if i >= 0 then
      var j = nums.length - 1
      while nums(j) <= nums(i) do j -= 1
      swap(i, j, nums)

    reverse(i + 1, nums)

  private def reverse(start: Int, nums: Array[Int]): Unit =
    var (i, j) = (start, nums.length - 1)
    while i < j do
      swap(i, j, nums)
      i += 1
      j -= 1

  private def swap(i: Int, j: Int, nums: Array[Int]): Unit =
    val tmp = nums(j)
    nums(j) = nums(i)
    nums(i) = tmp
