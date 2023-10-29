package com.leetcode.cosminci._2900

object _2899_LastVisitedIntegers:

  def lastVisitedIntegers(words: List[String]): List[Int] =
    words
      .foldLeft(Seq.empty[Int], Seq.empty[Int], 0) { case ((result, prevNums, prevCount), w) =>
        w match
          case "prev" =>
            val newPrevCount = prevCount + 1
            val numToAppend  = if prevNums.length < newPrevCount then -1 else prevNums(prevNums.length - newPrevCount)
            (result :+ numToAppend, prevNums, newPrevCount)
          case num =>
            (result, prevNums :+ num.toInt, 0)
      }._1.toList
