package com.leetcode.cosminci._2400

import scala.collection.mutable

object _2338_CountIdealArrays:

  def idealArrays(n: Int, maxValue: Int): Int =
    val mod = 1_000_000_007

    val mem0 = mutable.Map.empty[(Long, Long), Long]
    def comb(n: Long, k: Long): Long =
      mem0.getOrElseUpdate((n, k),
        if n < k then 0
        else if k == 0 || k == n then 1
        else (comb(n - 1, k - 1) + comb(n - 1, k)) % mod
      )

    val mem1 = mutable.Map.empty[(Long, Long), Long]
    def dfs(lastValue: Long, seqLength: Long): Long =
      mem1.getOrElseUpdate((lastValue, seqLength), {
        val res = if seqLength > 0 then comb(n - 1, seqLength - 1) else 0
        if seqLength == n then res
        else
          (2 * lastValue to maxValue by lastValue)
            .foldLeft(res)((res, nv) => res + dfs(nv, seqLength + 1) % mod)
      })

    ((dfs(lastValue = 1, seqLength = 0) + dfs(lastValue = 1, seqLength = 1)) % mod).toInt
