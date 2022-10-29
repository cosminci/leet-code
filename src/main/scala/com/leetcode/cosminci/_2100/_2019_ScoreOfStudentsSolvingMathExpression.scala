package com.leetcode.cosminci._2100

import scala.collection.mutable

object _2019_ScoreOfStudentsSolvingMathExpression:
  def main(args: Array[String]): Unit =
    println(scoreOfStudents("7+3*1*2", Array(20, 13, 42)))
    println(scoreOfStudents("3+5*2", Array(13, 0, 10, 13, 13, 16, 16)))
    println(scoreOfStudents("6+0*1", Array(12, 9, 6, 4, 8, 6)))

  def scoreOfStudents(s: String, answers: Array[Int]): Int =
    val correctAnswer = (1 to s.length by 2)
      .filter(i => i == s.length || s(i) == '+')
      .foldLeft(0, 0) { case ((sum, j), i) =>
        val localProduct = (j until i by 2).foldLeft(1) { case (product, k) =>
          product * (s(k) - '0')
        }
        (sum + localProduct, i + 1)
      }
      ._1

    val mem = mutable.Map.empty[(Int, Int), Set[Int]]

    def dfs(startIdx: Int, endIdx: Int): Set[Int] =
      if endIdx - startIdx == 1 then return Set(s(startIdx) - '0')
      if mem.contains((startIdx, endIdx)) then return mem((startIdx, endIdx))

      val results = for
        splitIdx    <- startIdx + 1 until endIdx by 2
        leftResult  <- dfs(startIdx, splitIdx)
        rightResult <- dfs(splitIdx + 1, endIdx)
        result = if s(splitIdx) == '+' then leftResult + rightResult else leftResult * rightResult
        if result <= 1000
      yield result

      mem.update((startIdx, endIdx), results.toSet)
      mem((startIdx, endIdx))

    val results = dfs(startIdx = 0, endIdx = s.length)

    answers.map { answer =>
      if answer == correctAnswer then 5
      else if results.contains(answer) then 2
      else 0
    }.sum
