package com.leetcode.cosminci._3100

import scala.util.chaining.*

object _3039_ApplyOpsToMakeStringEmpty:

  def lastNonEmptyString(s: String): String =
    val counts = s.groupMapReduce(identity)(_ => 1)(_ + _)
    val max    = counts.values.max
    s.foldRight("", counts) { case (ch, (res, counts)) =>
      if counts(ch) != max then (res, counts)
      else (s"$ch$res", counts.updated(ch, counts(ch) - 1))
    }.pipe { case (res, _) => res }
