package com.leetcode.cosminci._3000

object _2947_CountBeautifulSubstringsI:

  def beautifulSubstrings(s: String, k: Int): Int =
    val vowelPrefixCount = s.scanLeft(0)((cnt, ch) => if "aeiou".contains(ch) then cnt + 1 else cnt)
    val consPredixCount  = s.scanLeft(0)((cnt, ch) => if "aeiou".contains(ch) then cnt else cnt + 1)
    (2 to s.length).foldLeft(0) { (res, j) =>
      (0 until j).foldLeft(res) { (res, i) =>
        val vowelCount = vowelPrefixCount(j) - vowelPrefixCount(i)
        val consCount  = consPredixCount(j) - consPredixCount(i)
        if vowelCount == consCount && (vowelCount * consCount) % k == 0 then res + 1 else res
      }
    }
