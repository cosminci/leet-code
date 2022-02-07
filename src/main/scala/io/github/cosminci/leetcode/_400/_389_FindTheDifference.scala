package io.github.cosminci.leetcode._400

object _389_FindTheDifference {
  def findTheDifference(s: String, t: String): Char = t.diff(s).head
}
