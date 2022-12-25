package com.leetcode.cosminci._2600

import scala.util.chaining.*

object _2514_CountAnagrams:

  private val mod = 1_000_000_007

  def countAnagrams(s: String): Int =
    val maxLen = s.split(' ').map(_.length).max
    val fact   = (1 to maxLen).scanLeft(1L)((fact, i) => fact * i % mod)
    val ifact  = fact.map(v => powMod(v, mod - 2) % mod)

    s.split(' ').foldLeft(0L) { (res, w) =>
      res * w.groupMapReduce(identity)(_ => 1)(_ + _).foldLeft(fact(w.length)) { case (numComb, (_, count)) =>
        numComb * ifact(count) % mod
      } % mod
    }.toInt

  private def powMod(n: Long, pow: Int) =
    Iterator
      .iterate((1L, n, pow)) { case (r, n, p) => (if p % 2 == 0 then r else r * n % mod, n * n % mod, p / 2) }
      .dropWhile { case (_, _, pow) => pow >= 1 }
      .next()
      .pipe { case (res, _, _) => res }
