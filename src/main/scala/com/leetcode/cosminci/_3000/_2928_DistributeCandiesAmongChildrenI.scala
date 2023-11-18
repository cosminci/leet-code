package com.leetcode.cosminci._3000

object _2928_DistributeCandiesAmongChildrenI:

  def distributeCandies(n: Int, limit: Int): Int =
    (0.max(n - 2 * limit) to limit.min(n)).foldLeft(0) { (res, i) =>
      res + limit.min(n - i) - 0.max(n - i - limit) + 1
    }
