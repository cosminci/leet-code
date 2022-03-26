package io.github.cosminci.leetcode._800

object _704_BinarySearch:
  def main(args: Array[String]): Unit =
    println(search(Array(-1, 0, 3, 5, 9, 12), 9))
    println(search(Array(-1, 0, 3, 5, 9, 12), 2))

  def search(nums: Array[Int], target: Int): Int =
    @annotation.tailrec
    def dfs(l: Int, r: Int): Int =
      if l >= r then Option.when(nums(l) == target)(l).getOrElse(-1)
      else
        val mid = l + (r - l) / 2
        if nums(mid) < target then dfs(l = mid + 1, r)
        else dfs(l, r = mid)

    dfs(l = 0, r = nums.length - 1)
