package io.github.cosminci.leetcode._2300

object _2262_TotalAppealOfAString:

  def appealSum(s: String): Long =
    s.indices.foldLeft(Array.fill(26)(-1), 0L) {
      case ((prevPos, totalAppeal), i) =>
        val appeal = (i - prevPos(s(i) - 'a')) * (s.length - i)
        (prevPos.updated(s(i) - 'a', i), totalAppeal + appeal)
      }._2
