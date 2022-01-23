package io.github.cosminci.leetcode._2200

import scala.collection.mutable

object _2140_SolvingQuestionsWithBrainpower:

  def mostPoints(questions: Array[Array[Int]]): Long =
    val mem = mutable.Map.empty[Int, Long]

    def dfs(idx: Int): Long = mem.getOrElseUpdate(idx, {
      if idx >= questions.length then 0
      else
        val Array(points, brainpower) = questions(idx)
        dfs(idx + 1).max(points + dfs(idx + brainpower + 1))
    })

    dfs(idx = 0)
