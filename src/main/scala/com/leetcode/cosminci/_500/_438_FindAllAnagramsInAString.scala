package com.leetcode.cosminci._500

import com.leetcode.cosminci.utils

import scala.collection.mutable

object _438_FindAllAnagramsInAString:
  def main(args: Array[String]): Unit =
    println(findAnagrams("cbaebabacd", "abc"))
    println(findAnagrams("abab", "ab"))

  def findAnagrams(s: String, p: String): List[Int] =
    val pChars     = utils.characterCounts(p)
    val firstChars = utils.characterCounts(s.take(p.length - 1))

    (p.length - 1 until s.length)
      .foldLeft(Seq.empty[Int], firstChars) {
        case ((result, prevChars), sIdx) =>
          val newChar    = s(sIdx) - 'a'
          val oldChar    = s(sIdx - p.length + 1) - 'a'
          val currChars  = prevChars.updated(newChar, prevChars(newChar) + 1)
          val nextChars  = currChars.updated(oldChar, currChars(oldChar) - 1)
          val nextResult = Option.when(currChars == pChars)(result :+ (sIdx - p.length + 1)).getOrElse(result)
          (nextResult, nextChars)
      }._1.toList
