package com.leetcode.cosminci._3000

object _2929_DistributeCandiesAmongChildrenII:

  def distributeCandies(n: Int, limit: Int): Long =
    (0.max(n - 2 * limit) to limit.min(n)).foldLeft(0L) { (res, i) =>
      res + limit.min(n - i) - 0.max(n - i - limit) + 1
    }
