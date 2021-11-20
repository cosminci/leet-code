package io.github.cosminci.leetcode._600

object _540_SingleElementInSortedArray:
  def main(args: Array[String]): Unit =
    println(singleNonDuplicate(Array(1, 1, 2, 3, 3, 4, 4, 8, 8)))
    println(singleNonDuplicate(Array(3, 3, 7, 7, 10, 11, 11)))

  def singleNonDuplicate(nums: Array[Int]): Int =
    @annotation.tailrec
    def dfs(l: Int, r: Int): Int =
      if l >= r then nums(l)
      else
        val mid = l + (r - l) / 2
        if nums(mid) != nums(mid ^ 1) then dfs(l, mid)
        else dfs(mid + 1, r)

    dfs(l = 0, r = nums.length - 1)
