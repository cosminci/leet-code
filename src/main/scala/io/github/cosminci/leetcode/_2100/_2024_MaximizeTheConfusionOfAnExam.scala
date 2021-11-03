package io.github.cosminci.leetcode._2100

import scala.collection.mutable

object _2024_MaximizeTheConfusionOfAnExam:
  def main(args: Array[String]): Unit =
    println(maxConsecutiveAnswers("TTFF", 2))
    println(maxConsecutiveAnswers("TFFT", 1))
    println(maxConsecutiveAnswers("TTFTTFTT", 1))

  def maxConsecutiveAnswers(answerKey: String, k: Int): Int =
    def max(char: Char): Int =
      var (mismatchCount, left, result) = (0, 0, 0)
      answerKey.indices.foreach { right =>
        if answerKey(right) != char then mismatchCount += 1
        while mismatchCount > k do
          if answerKey(left) != char then mismatchCount -= 1
          left += 1
        result = math.max(result, right - left + 1)
      }
      result

    math.max(max('T'), max('F'))
