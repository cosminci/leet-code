package com.leetcode.cosminci._800

object _744_FindSmallestLetterGreaterThanTarget:
  def main(args: Array[String]): Unit =
    println(nextGreatestLetter(Array('c', 'f', 'j'), 'j'))
    
  def nextGreatestLetter(letters: Array[Char], target: Char): Char =
    var (l, r) = (0, letters.length)
    while l < r do
      val mid = l + (r - l) / 2
      if letters(mid) <= target then l = mid + 1
      else r = mid
    letters(l % letters.length)
