package com.leetcode.cosminci._1700

import scala.collection.mutable
import scala.util.chaining.*

object _1601_MaxAchievableTransferRequests:

  def maximumRequests(n: Int, requests: Array[Array[Int]]): Int =
    val mem = mutable.Map.empty[(Int, Seq[Int]), Int]
    def dfs(i: Int, balance: Seq[Int]): Int = mem.getOrElseUpdate((i, balance),
      if i == requests.length then if balance.forall(_ == 0) then 0 else Int.MinValue
      else
        val Array(from, to) = requests(i)
        val takeBalance     = balance.updated(from, balance(from) - 1).pipe(b => b.updated(to, b(to) + 1))
        dfs(i + 1, balance).max(1 + dfs(i + 1, takeBalance))
    )
    dfs(i = 0, balance = Seq.fill(n)(0))
