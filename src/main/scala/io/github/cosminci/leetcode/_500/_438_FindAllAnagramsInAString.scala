package io.github.cosminci.leetcode._500

import io.github.cosminci.utils

import scala.collection.mutable

object _438_FindAllAnagramsInAString:
  def main(args: Array[String]): Unit =
    println(findAnagrams("cbaebabacd", "abc"))
    println(findAnagrams("abab", "ab"))

  def findAnagrams(s: String, p: String): List[Int] =
    if p.length > s.length then return List.empty

    val sChars  = utils.characterCounts(s.take(p.length)).toArray
    val pChars  = utils.characterCounts(p).toArray
    val results = mutable.ListBuffer.empty[Int]
    if sChars sameElements pChars then results.append(0)

    (p.length until s.length).foreach { sIdx =>
      sChars(s(sIdx - p.length) - 'a') -= 1
      sChars(s(sIdx) - 'a') += 1
      if sChars sameElements pChars then results.append(sIdx - p.length + 1)
    }

    results.toList
