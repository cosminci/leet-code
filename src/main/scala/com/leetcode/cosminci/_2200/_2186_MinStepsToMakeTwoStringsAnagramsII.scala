package com.leetcode.cosminci._2200

object _2186_MinStepsToMakeTwoStringsAnagramsII:
  def minSteps(s: String, t: String): Int =
    def countChars(s: String) = s.groupMapReduce(identity)(_ => 1)(_ + _)

    def diffChars(s1Chars: Map[Char, Int], s2Chars: Map[Char, Int]) =
      s1Chars.foldLeft(0) { case (acc, (char, count)) =>
        acc + (count - s2Chars.getOrElse(char, 0)).max(0)
      }

    val (sChars, tChars) = (countChars(s), countChars(t))
    diffChars(sChars, tChars) + diffChars(tChars, sChars)
