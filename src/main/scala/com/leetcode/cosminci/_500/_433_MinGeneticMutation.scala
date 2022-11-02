package com.leetcode.cosminci._500

object _433_MinGeneticMutation:

  def minMutation(start: String, end: String, bank: Array[String]): Int =
    def nextSearchState(toVisit: Seq[(String, Int)], visited: Set[String]) =
      val (curr, steps) +: tail = toVisit
      bank
        .filterNot(visited.contains)
        .filter { next => curr.zip(next).count { case (base1, base2) => base1 != base2 } == 1 }
        .foldLeft((tail, visited)) { case ((toVisit, visited), next) =>
          (toVisit :+ (next, steps + 1), visited + next)
        }

    Iterator
      .iterate((Seq((start, 0)), Set(start)))((nextSearchState _).tupled)
      .takeWhile { case (toVisit, _) => toVisit.nonEmpty }
      .flatMap { case (toVisit, _) => toVisit.headOption }
      .collectFirst { case (mutation, steps) if mutation == end => steps }
      .getOrElse(-1)
