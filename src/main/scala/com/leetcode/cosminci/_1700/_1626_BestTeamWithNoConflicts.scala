package com.leetcode.cosminci._1700

import scala.collection.mutable

object _1626_BestTeamWithNoConflicts:

  def bestTeamScore(scores: Array[Int], ages: Array[Int]): Int =
    val (playerAges, playerScores) = ages.zip(scores).sorted.unzip

    val mem = mutable.Map.empty[Int, Int]
    def dfs(i: Int): Int = mem.getOrElseUpdate(i,
      playerScores(i) + (0 until i)
        .collect { case j if playerAges(j) == playerAges(i) || playerScores(j) <= playerScores(i) => dfs(j) }
        .maxOption
        .getOrElse(0)
    )

    scores.indices.map(dfs).max
