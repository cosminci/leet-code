package com.leetcode.cosminci._3100

object _3029_MinTimeToRevertWordToInitialStateI:

  def minimumTimeToInitialState(word: String, k: Int): Int =
    val z = zFunction(word)
    (k until word.length by k)
      .collectFirst { case i if z(i) == word.length - i => i / k }
      .getOrElse((word.length + k - 1) / k)

  private def zFunction(s: String): Vector[Int] =
    @annotation.tailrec
    def dfs(i: Int, l: Int, r: Int, z: Vector[Int]): Vector[Int] =
      if i == s.length then z
      else
        val zi = if i <= r then z(i - l).min(r - i + 1) else 0
        val j  = Iterator.iterate(zi + i)(_ + 1).dropWhile(j => j < s.length && s(j) == s(j - i)).next()
        val (newL, newR) = if j > r then (i, j - 1) else (l, r)
        dfs(i + 1, newL, newR, z :+ (j - i))

    dfs(i = 1, l = 0, r = 0, z = Vector(s.length))
