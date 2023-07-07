package com.leetcode.cosminci._2100

import scala.util.chaining.*

object _2024_MaximizeTheConfusionOfAnExam:

  def maxConsecutiveAnswers(answerKey: String, k: Int): Int =
    def max(char: Char): Int =
      answerKey.indices
        .foldLeft(0, 0, 0) { case ((res, cnt, l), r) =>
          val cost = if answerKey(r) != char then 1 else 0
          Iterator
            .iterate((cnt + cost, l)) { case (cnt, l) => (cnt - (if answerKey(l) != char then 1 else 0), l + 1) }
            .dropWhile { case (cnt, _) => cnt > k }.next()
            .pipe { case (cnt, l) => (res.max(r - l + 1), cnt, l) }
        }.pipe { case (res, _, _) => res }

    max(char = 'T') max max(char = 'F')
