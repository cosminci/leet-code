package io.github.cosminci.leetcode._2300

import scala.collection.mutable

object _2218_MaxValueOfKCoinsFromPiles:

  def maxValueOfCoins(piles: List[List[Int]], k: Int): Int =
    val mem = mutable.Map.empty[(Int, Int), Int]

    def dfs(i: Int, k: Int): Int = mem.getOrElseUpdate((i, k), {
      if k == 0 || i == piles.length then 0
      else
        (0 until k.min(piles(i).length))
          .foldLeft(dfs(i + 1, k), 0) { case ((res, sum), j) =>
            val newSum = sum + piles(i)(j)
            val newRes = res.max(newSum + dfs(i + 1, k - j - 1))
            (newRes, newSum)
          }._1
    })

    dfs(i = 0, k)
