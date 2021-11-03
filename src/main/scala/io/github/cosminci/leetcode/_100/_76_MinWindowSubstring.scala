package io.github.cosminci.leetcode._100

import scala.collection.mutable

object _76_MinWindowSubstring:
  def main(args: Array[String]): Unit =
    println(minWindow("ADOBECODEBANC", "ABC"))
    println(minWindow("a", "a"))
    println(minWindow("a", "aa"))
    println(minWindow("a", "b"))

  def minWindow(s: String, t: String): String =
    if t.length > s.length then return ""

    val tChars = t.foldLeft(Map.empty[Char, Int]) { case (counts, char) =>
      counts.updated(char, counts.getOrElse(char, 0) + 1)
    }

    val tCharsConsumable = mutable.Map.from(tChars)
    var (l, r)           = (0, 0)
    val windowChars      = mutable.Map.empty[Char, Int]
    // move r to the right until the window contains all target chars
    while r < s.length && tCharsConsumable.nonEmpty do
      if tChars.contains(s(r)) then
        tCharsConsumable.updateWith(s(r)) {
          case None | Some(1) => None
          case Some(n)        => Some(n - 1)
        }
        windowChars.update(s(r), windowChars.getOrElseUpdate(s(r), 0) + 1)
      r += 1
    // right reached the end of s but not all t chars were matched
    if tCharsConsumable.nonEmpty then return ""

    var maxWindow = (l, r)
    // reduce and expand the initial window trying to find a smaller size
    while true do
      if !tChars.contains(s(l)) then
        // just move left when leftmost character is not in target
        l += 1
        if r - l < maxWindow._2 - maxWindow._1 then maxWindow = (l, r)
      else if windowChars(s(l)) > tChars(s(l)) then
        // reduce the window count if leftmost character is in target, and there is a surplus
        windowChars.update(s(l), windowChars(s(l)) - 1)
        l += 1
        if r - l < maxWindow._2 - maxWindow._1 then maxWindow = (l, r)
      else {
        // no surplus and leftmost character is in target -> move right until we find a replacement
        while r < s.length && s(r) != s(l) do
          if tChars.contains(s(r)) then
            // while finding the leftmost, we can find other target characters - add them as surplus
            windowChars.update(s(r), windowChars(s(r)) + 1)
          r += 1
        if r == s.length || s(r) != s(l) then
          // reached end and found no replacement for leftmost character - return
          return s.substring(maxWindow._1, maxWindow._2)
        r += 1
        l += 1
      }
    ""
