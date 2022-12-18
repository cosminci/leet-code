package com.leetcode.cosminci._2600

import scala.util.chaining.*

object _2509_CycleLenghQueriesInTree:

  def cycleLengthQueries(n: Int, queries: Array[Array[Int]]): Array[Int] =
    queries.map { case Array(q1, q2) =>
      Iterator
        .iterate((q1, q2, 1)) { case (q1, q2, lcaDist) => (q1 min q2, (q1 max q2) / 2, lcaDist + 1) }
        .dropWhile { case (q1, q2, _) => q1 != q2 }
        .next()
        .pipe { case (_, _, lcaDist) => lcaDist }
    }
