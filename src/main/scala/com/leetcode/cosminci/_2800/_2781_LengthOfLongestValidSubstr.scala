package com.leetcode.cosminci._2800

import scala.util.chaining.*

object _2781_LengthOfLongestValidSubstr:

  def longestValidSubstring(word: String, forbidden: List[String]): Int =
    val forbiddenSet = forbidden.toSet

    word.indices.reverse
      .foldLeft(0, word.indices.last) { case ((res, r), l) =>
        (l to (l + 9).min(r))
          .collectFirst { case k if forbiddenSet.contains(word.substring(l, k + 1)) => k - 1 }
          .getOrElse(r)
          .pipe(r => (res.max(r - l + 1), r))
      }
      .pipe { case (res, _) => res }
