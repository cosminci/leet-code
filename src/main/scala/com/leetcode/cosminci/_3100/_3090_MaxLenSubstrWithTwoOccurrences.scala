package com.leetcode.cosminci._3100

import scala.util.chaining.*

object _3090_MaxLenSubstrWithTwoOccurrences:

  def maximumLengthSubstring(s: String): Int =
    s.indices
      .foldLeft(0, 0, Map.empty[Char, Int].withDefaultValue(0)) { case ((res, l, ctr), r) =>
        Iterator
          .iterate((l, ctr.updated(s(r), ctr(s(r)) + 1))) { case (l, ctr) => (l + 1, ctr.updated(s(l), ctr(s(l)) - 1)) }
          .dropWhile { case (l, ctr) => ctr(s(r)) > 2 }.next()
          .pipe { case (l, ctr) => (res.max(r - l + 1), l, ctr) }
      }.pipe { case (res, _, _) => res }
