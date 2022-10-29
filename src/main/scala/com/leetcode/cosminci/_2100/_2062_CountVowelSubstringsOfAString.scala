package com.leetcode.cosminci._2100

import scala.collection.mutable

object _2062_CountVowelSubstringsOfAString:
  def main(args: Array[String]): Unit =
    Seq("aeiouu", "unicornarihan", "cuaieuouac", "bbaeixoubb").foreach(s => println(countVowelSubstrings(s)))

  def countVowelSubstrings(word: String): Int =
    val vowels = Set('a', 'e', 'i', 'o', 'u')

    @annotation.tailrec
    def shrinkWindow(allVowelEnd: Int, counts: Map[Char, Int]): (Int, Map[Char, Int]) =
      if counts.size != vowels.size then
        (allVowelEnd, counts)
      else
        shrinkWindow(allVowelEnd + 1, counts.updatedWith(word(allVowelEnd)) {
          case None | Some(1) => None
          case Some(c)        => Some(c - 1)
        })

    word.indices.foldLeft(0, 0, 0, Map.empty[Char, Int]) {
      case ((result, allVowelStart, allVowelEnd, prevCounts), idx) =>
        if !vowels.contains(word(idx)) then
          (result, idx + 1, idx + 1, Map.empty)
        else {
          val countsWithNewChar   = prevCounts.updated(word(idx), prevCounts.getOrElse(word(idx), 0) + 1)
          val (newEnd, newCounts) = shrinkWindow(allVowelEnd, countsWithNewChar)
          (result + newEnd - allVowelStart, allVowelStart, newEnd, newCounts)
        }
      }
      ._1
