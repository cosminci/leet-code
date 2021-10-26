package io.github.cosminci.leetcode._400

object _374_GuessNumber:
  private def guessNumber(n: Int): Int =
    var (l, r) = (0, n)
    while l < r do
      val mid = l + (r - l) / 2
      if guess(mid) == 0 then return mid
      else if guess(mid) < 0 then r = mid
      else l = mid + 1
    l

  private def guess(n: Int): Int = n.compare(5)
