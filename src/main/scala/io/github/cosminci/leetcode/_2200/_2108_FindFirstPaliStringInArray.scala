package io.github.cosminci.leetcode._2200

object _2108_FindFirstPaliStringInArray:

  def firstPalindrome(words: Array[String]): String =
    @annotation.tailrec
    def isPali(w: String, l: Int, r: Int): Boolean =
      if l >= r then true
      else w(l) == w(r) && isPali(w, l + 1, r - 1)

    words.find(w => isPali(w, 0, w.length - 1)).getOrElse("")
