package com.leetcode.cosminci._3100

object _3048_EarliestSecondToMarkIndicesI {

  def earliestSecondToMarkIndices(nums: Array[Int], changeIndices: Array[Int]): Int = {
    val (n, m) = (nums.length, changeIndices.length)

    def canMarkAll(idx: Int): Boolean = {
      val last = changeIndices.slice(0, idx).zipWithIndex.map { case (num, i) => num -> i }.toMap

      @annotation.tailrec
      def dfs(i: Int, cnt: Int): Boolean =
        if (i == idx) true
        else if (i != last(changeIndices(i))) dfs(i + 1, cnt + 1)
        else if (cnt < nums(changeIndices(i) - 1)) false
        else dfs(i + 1, cnt - nums(changeIndices(i) - 1))

      if (last.size != nums.length) false
      else dfs(i = 0, cnt = 0)
    }


    @annotation.tailrec
    def search(l: Int, r: Int): Int =
      if (l >= r) if (r == m + 1) -1 else r
      else {
        val mid = l + (r - l) / 2
        if (canMarkAll(mid)) search(l, mid) else search(mid + 1, r)
      }

    search(l = 0, r = m + 1)
  }

}
