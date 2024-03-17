package com.leetcode.cosminci._3100

object _3044_MostFrequentPrime:

  def mostFrequentPrime(mat: Array[Array[Int]]): Int =
    val startStates = for
      r  <- mat.indices
      c  <- mat(r).indices
      dr <- -1 to 1
      dc <- -1 to 1
      if dr != 0 || dc != 0
    yield (r, c, dr, dc)

    val nums = startStates.flatMap { case (r, c, dr, dc) =>
      Iterator
        .iterate((r, c)) { case (r, c) => (r + dr, c + dc) }
        .takeWhile { case (r, c) => r >= 0 && r < mat.length && c >= 0 && c < mat(r).length }
        .scanLeft(0) { case (num, (r, c)) => num * 10 + mat(r)(c) }
    }

    nums
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .filter { case (num, cnt) => isPrime(num) }
      .maxByOption { case (num, cnt) => (cnt, num) }
      .map { case (num, _) => num }.getOrElse(-1)

  private def isPrime(n: Int): Boolean =
    n > 10 && n % 2 != 0 && (3 to math.sqrt(n).toInt by 2).forall(n % _ != 0)
