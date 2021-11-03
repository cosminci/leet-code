package io.github.cosminci.leetcode._2000

import scala.collection.mutable

object _1930_UniqueLengthThreePalindromicSubsequence:
  def main(args: Array[String]): Unit =
    println(countPalindromicSubsequence("aabca"))
    println(countPalindromicSubsequence2("aabca"))

  def countPalindromicSubsequence2(s: String): Int =
    val charsLeft = mutable.Map.from(s.groupBy(identity).view.mapValues(_.length))
    val charsSeen = mutable.Set(s.head)
    charsSeen.add(s.head)
    removeChar(s.head, charsLeft)

    val palindromes = mutable.Set.empty[String]
    (1 until s.length - 1).foreach { idx =>
      removeChar(s(idx), charsLeft)
      charsSeen.foreach { c =>
        if charsLeft.contains(c) then palindromes.add(s"$c${s(idx)}")
      }
      charsSeen.add(s(idx))
    }
    palindromes.size

  def removeChar(c: Char, charsLeft: mutable.Map[Char, Int]) =
    charsLeft.updateWith(c) {
      case None              => None
      case Some(c) if c == 1 => None
      case Some(c)           => Some(c - 1)
    }

  def countPalindromicSubsequence(s: String): Int =
    val found          = mutable.Set.empty[(Int, Int)]
    val visitedLetters = mutable.Set.empty[Char]
    s.zipWithIndex.foreach { case (c, idx) =>
      if !visitedLetters.contains(c) then
        visitedLetters.add(c)
        var i       = s.length - 1
        var matched = false
        while i > idx + 1 && !matched do
          if c == s.charAt(i) then
            found.add((idx, i))
            matched = true
          i -= 1
    }
    val palindromes = mutable.Set.empty[String]
    found.foreach { case (start, end) =>
      s.substring(start + 1, end).toCharArray.foreach { c =>
        palindromes.add(s"${s(start)}$c")
      }
    }
    palindromes.size
