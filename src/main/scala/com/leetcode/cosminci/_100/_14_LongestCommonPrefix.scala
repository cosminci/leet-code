package com.leetcode.cosminci._100

object _14_LongestCommonPrefix:
  def main(args: Array[String]): Unit =
    println(longestCommonPrefix(Array("flowe", "flower", "flower", "flower")))
    println(longestCommonPrefixBinarySearch(Array("flowe", "flower", "flower", "flower")))

  def longestCommonPrefix(strs: Array[String]): String =
    if strs.length == 1 then return strs.head
    if strs.exists(_.isEmpty) then return ""

    var i = 0
    while true do
      val chars = strs.map(s => if s.isDefinedAt(i) then s(i) else '0')

      var j = 1
      while j < chars.length && chars.head == chars(j) do j += 1
      if chars.contains('0') then return strs.head.take(i)
      if j == strs.length then i += 1
    ""

  def longestCommonPrefixBinarySearch(strs: Array[String]): String =
    def binarySearch(min: Int, max: Int): String =
      var (l, r) = (min, max)
      while l < r do
        val mid = l + (r - l) / 2
        if strs.map(_.take(mid)).toSet.size == 1 then l = mid + 1
        else r = mid
      strs.head.take(l - 1)

    binarySearch(0, strs.map(_.length).min + 1)
