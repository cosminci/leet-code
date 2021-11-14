package io.github.cosminci.leetcode._2100

import io.github.cosminci.utils

object _2068_CheckWhetherTwoStringsAreAlmostEquivalent:

  def checkAlmostEquivalent(word1: String, word2: String): Boolean =
    utils.characterCounts(word1).zip(utils.characterCounts(word2)).forall((c1, c2) => math.abs(c1 - c2) <= 3)
