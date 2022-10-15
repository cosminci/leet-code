package io.github.cosminci.leetcode._2500

object _2438_RangeProductQueriesOfPowers:

  def productQueries(n: Int, queries: Array[Array[Int]]): Array[Int] =
    val mod = 1_000_000_007
    val powers = n.toBinaryString.reverse.zipWithIndex.foldLeft(Array.empty[Long]) {
      case (powers, (bit, pow)) =>
        if bit == '0' then powers else powers :+ (1 << pow)
    }
    queries.map { case Array(l, r) =>
      (l to r).map(powers).reduce(_ * _ % mod)
    }.map(_.toInt)
