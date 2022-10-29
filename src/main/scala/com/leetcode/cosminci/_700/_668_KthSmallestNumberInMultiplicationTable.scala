package com.leetcode.cosminci._700

object _668_KthSmallestNumberInMultiplicationTable:
  def main(args: Array[String]): Unit =
    println(findKthNumber(3, 3, 5))
    println(findKthNumber(2, 3, 6))

  def findKthNumber(m: Int, n: Int, k: Int): Int =
    def enough(value: Int) =
      (1 to m).map(row => n.min(value / row)).sum >= k

    @annotation.tailrec
    def dfs(l: Int, r: Int): Int =
      if l >= r then l
      else
        val mid = l + (r - l) / 2
        if enough(mid) then dfs(l, mid)
        else dfs(mid + 1, r)

    dfs(l = 1, r = m * n)
