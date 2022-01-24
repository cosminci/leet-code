package io.github.cosminci.leetcode._600

object _520_DetectCapital {

  def detectCapitalUse(word: String): Boolean =
    word.forall(_.isLower) ||
      word.forall(_.isUpper) ||
      (word.head.isUpper && word.tail.forall(_.isLower))
}
