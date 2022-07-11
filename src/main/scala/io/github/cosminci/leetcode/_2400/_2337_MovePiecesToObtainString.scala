package io.github.cosminci.leetcode._2400

object _2337_MovePiecesToObtainString:

  def canChange(start: String, target: String): Boolean =
    def indicesOf(s: String, c: Char) = s.indices.filter(i => s(i) == c)

    start.filterNot(_ == '_') == target.filterNot(_ == '_') &&
    indicesOf(start, 'L').corresponds(indicesOf(target, 'L'))(_ >= _) &&
    indicesOf(start, 'R').corresponds(indicesOf(target, 'R'))(_ <= _)
