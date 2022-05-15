package io.github.cosminci.leetcode._2300

object _2272_SubstringWithLargestVariance:

  def largestVariance(s: String): Int =
    val freq = s.groupMapReduce(identity)(_ => 1)(_ + _)

    s.distinct.toArray
      .combinations(2)
      .flatMap(arr => Array(arr, arr.reverse))
      .foldLeft(0) { case (maxVariance, Array(a, b)) =>
        s.foldLeft(maxVariance, freq(a), 0, 0) { case ((maxVariance, remA, currA, currB), c) =>
          val newCurrB            = if c == b then currB + 1 else currB
          val (newRemA, newCurrA) = if c == a then (remA - 1, currA + 1) else (remA, currA)
          val newMaxVariance      = if newCurrA > 0 then maxVariance.max(newCurrB - newCurrA) else maxVariance
          if newCurrB < newCurrA && newRemA >= 1 then (newMaxVariance, newRemA, 0, 0)
          else (newMaxVariance, newRemA, newCurrA, newCurrB)
        }._1
      }
