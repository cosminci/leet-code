package com.leetcode.cosminci._1900

object _1898_MaxNumberOfRemovableChars:
  def main(args: Array[String]): Unit =
    println(maximumRemovals("abcacb", "ab", Array(3, 1, 0)))
    println(maximumRemovals("abcbddddd", "abcd", Array(3, 2, 1, 4, 5, 6)))
    println(maximumRemovals("qlevcvgzfpryiqlwy", "qlecfqlw", Array(12, 5)))

  def maximumRemovals(s: String, p: String, removable: Array[Int]): Int =
    var (l, r) = (0, removable.length - 1)
    var max    = 0
    while l <= r do
      val mid     = (l + r) / 2
      val removed = removable.take(mid + 1).toSet
      if isSubsequence(s, p, removed) then
        max = math.max(mid + 1, max)
        l = mid + 1
      else r = mid - 1
    max

  def isSubsequence(s: String, p: String, removed: Set[Int]): Boolean =
    var pIdx = p.length - 1
    var sIdx = s.length - 1
    while pIdx >= 0 && sIdx >= 0 do
      if !removed.contains(sIdx) && s(sIdx) == p(pIdx) then pIdx -= 1
      sIdx -= 1
    pIdx == -1
