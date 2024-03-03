package com.leetcode.cosminci._3100

import scala.util.chaining.*

object _3045_CountPrefixAndSuffixPairsII:

  def countPrefixSuffixPairs(words: Array[String]): Long =
    words
      .foldLeft(0L, Map.empty[String, Long].withDefaultValue(0L), Set.empty[Int]) { case ((res, strCnt, seenLen), w) =>
        val z = zFunction(w)
        val newRes = (1 until w.length).foldLeft(res) { (res, i) =>
          if (i + z(i)) != w.length || !seenLen.contains(z(i)) then res
          else res + strCnt(w.slice(0, z(i)))
        }
        (newRes + strCnt.getOrElse(w, 0L), strCnt.updated(w, strCnt(w) + 1), seenLen + w.length)
      }.pipe { case (res, _, _) => res }

  private def zFunction(s: String): Vector[Int] =
    @annotation.tailrec
    def dfs(i: Int, l: Int, r: Int, z: Vector[Int]): Vector[Int] =
      if i == s.length then z
      else
        val zi           = if i <= r then z(i - l).min(r - i + 1) else 0
        val j            = Iterator.iterate(zi + i)(_ + 1).dropWhile(j => j < s.length && s(j) == s(j - i)).next()
        val (newL, newR) = if j > r then (i, j - 1) else (l, r)
        dfs(i + 1, newL, newR, z :+ (j - i))

    dfs(i = 1, l = 0, r = 0, z = Vector(s.length))
