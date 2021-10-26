package io.github.cosminci.leetcode._300

import scala.collection.mutable

object _205_IsomorphicStrings:
  def main(args: Array[String]): Unit =
    println(isIsomorphic("abcdefghijklmnopqrstuvwxyzva", "abcdefghijklmnopqrstuvwxyzck"))
    println(isIsomorphic2("abcdefghijklmnopqrstuvwxyzva", "abcdefghijklmnopqrstuvwxyzck"))

  private def isIsomorphic(s: String, t: String): Boolean =
    if s.length != t.length then return false

    val charMappings        = mutable.Map.empty[Char, Char]
    val reverseCharMappings = mutable.Map.empty[Char, Char]

    s.indices.foreach { idx =>
      if charMappings.contains(s(idx)) then
        if charMappings(s(idx)) != t(idx) then return false
      else if reverseCharMappings.contains(t(idx)) then
        if reverseCharMappings(t(idx)) != s(idx) then return false
      else
        charMappings.update(s(idx), t(idx))
        reverseCharMappings.update(t(idx), s(idx))
    }

    true

  def isIsomorphic2(s: String, t: String): Boolean =
    charToFirstIndex(s) == charToFirstIndex(t)

  def charToFirstIndex(s: String): String =
    val firstIdx = mutable.Map.empty[Char, Int]
    val result   = new mutable.StringBuilder

    s.indices.foreach { idx =>
      firstIdx.get(s(idx)) match
        case Some(i) => result.append(i)
        case None =>
          result.append(idx)
          firstIdx.update(s(idx), idx)
      result.append(" ")
    }
    result.toString()
