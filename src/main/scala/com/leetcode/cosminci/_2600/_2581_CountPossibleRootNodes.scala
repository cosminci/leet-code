package com.leetcode.cosminci._2600

import scala.collection.mutable

object _2581_CountPossibleRootNodes:

  def rootCount(edges: Array[Array[Int]], guesses: Array[Array[Int]], k: Int): Int =
    val graph = edges.foldLeft(Map.empty[Int, Seq[Int]].withDefaultValue(Seq.empty)) { case (g, Array(i, j)) =>
      g.updated(i, g(i) :+ j).updated(j, g(j) :+ i)
    }
    val guessSet = guesses.map { case Array(i, j) => (i, j) }.toSet

    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(curr: Int, prev: Int): Int = mem.getOrElseUpdate((curr, prev),
      graph(curr)
        .filterNot(_ == prev)
        .foldLeft(0) { (correctCnt, next) =>
          val currCorrect = if guessSet.contains((curr, next)) then 1 else 0
          correctCnt + currCorrect + dfs(next, curr)
        }
    )

    graph.keys.foldLeft(0) { (res, i) =>
      if dfs(curr = i, prev = -1) >= k then res + 1 else res
    }
