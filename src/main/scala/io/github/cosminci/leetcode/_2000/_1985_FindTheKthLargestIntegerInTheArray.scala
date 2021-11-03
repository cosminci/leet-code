package io.github.cosminci.leetcode._2000

object _1985_FindTheKthLargestIntegerInTheArray:
  def main(args: Array[String]): Unit =
    println(kthLargestNumber(Array("3", "6", "7", "10"), 4))
    println(kthLargestNumber(Array("2", "21", "12", "1"), 4))
    println(kthLargestNumber(Array("0", "0"), 1))

  def kthLargestNumber(nums: Array[String], k: Int): String =
    def swap(i: Int, j: Int): Unit =
      val tmp = nums(i); nums(i) = nums(j); nums(j) = tmp

    def isLower(s1: String, s2: String): Boolean =
      if s1.length == s2.length then s1 < s2 else s1.length < s2.length

    def quickselect(start: Int, end: Int): String =
      val pivot  = nums(end)
      var (i, j) = (start, start)
      while j < end do
        if isLower(pivot, nums(j)) then
          swap(i, j)
          i += 1
        j += 1
      swap(i, end)

      if i == k - 1 then return nums(i)
      else if i > k - 1 then quickselect(start, i - 1)
      else quickselect(i + 1, end)

    quickselect(0, nums.length - 1)
