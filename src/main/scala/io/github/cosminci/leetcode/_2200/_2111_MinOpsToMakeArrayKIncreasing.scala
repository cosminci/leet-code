package io.github.cosminci.leetcode._2200

import scala.collection.Searching.*

object _2111_MinOpsToMakeArrayKIncreasing:
  def main(args: Array[String]): Unit =
    println(kIncreasing(Array.from(1 to 100000), 1))

  def kIncreasing(arr: Array[Int], k: Int): Int =
    arr.grouped(k).toArray.transpose.map(a => a.length - lengthOfLIS(a)).sum

  private def lengthOfLIS(nums: Array[Int]): Int =
    val monostack = Array.fill(nums.length + 1)(Int.MaxValue)
    nums.foreach(n => monostack(upperBound(n, monostack)) = n)
    monostack.indexWhere(_ == Int.MaxValue)

  private def upperBound(value: Int, stack: Array[Int]): Int =
    @annotation.tailrec
    def dfs(l: Int, r: Int): Int =
      if l >= r then l
      else
        val mid = l + (r - l) / 2
        if stack(mid) <= value then dfs(mid + 1, r)
        else dfs(l, mid)
    dfs(l = 0, r = stack.length - 1)
