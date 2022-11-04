package com.leetcode.cosminci._400

import scala.util.chaining.*

object _345_ReverseVowelsInString:

  def reverseVowels(s: String): String =
    Seq('a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U').pipe { allVowels =>
      s.filter(allVowels.contains).pipe { vowels =>
        s.foldLeft(Array.empty[Char], vowels.length - 1) { case ((result, i), ch) =>
          if allVowels.contains(ch) then (result :+ vowels(i), i - 1) else (result :+ ch, i)
        }._1.mkString
      }
    }
