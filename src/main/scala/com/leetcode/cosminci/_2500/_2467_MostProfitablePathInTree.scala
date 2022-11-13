package com.leetcode.cosminci._2500

import scala.util.chaining.*

object _2467_MostProfitablePathInTree:

  def mostProfitablePath(edges: Array[Array[Int]], bob: Int, amount: Array[Int]): Int =
    val graph = edges.foldLeft(Map.empty[Int, Set[Int]].withDefaultValue(Set.empty)) { case (graph, Array(x, y)) =>
      graph.updated(x, graph(x) + y).updated(y, graph(y) + x)
    }

    def dfsBob(curr: Int, prev: Int, parent: Map[Int, Int]): Array[Int] =
      parent.updated(curr, prev).pipe { parent =>
        if curr == 0 then
          Iterator
            .iterate((Array.empty[Int], curr)) { case (steps, curr) => (steps :+ curr, parent(curr)) }
            .dropWhile { case (_, curr) => curr != -1 }
            .next()
            .pipe { case (steps, _) => steps.reverse }
        else
          graph(curr)
            .filterNot(_ == prev)
            .map(dfsBob(_, curr, parent))
            .find(_.nonEmpty)
            .getOrElse(Array.empty)
      }

    val timeBob = dfsBob(curr = bob, prev = -1, parent = Map.empty).zipWithIndex.toMap

    def dfsAlice(curr: Int, prev: Int, time: Int): Int =
      graph(curr)
        .filterNot(_ == prev)
        .map(dfsAlice(_, curr, time + 1))
        .maxOption
        .getOrElse(0) + income(curr, time)

    def income(pos: Int, timeAlice: Int): Int =
      timeBob.get(pos) match
        case Some(time) if time == timeAlice => amount(pos) / 2
        case Some(time) if time < timeAlice  => 0
        case _                               => amount(pos)

    dfsAlice(curr = 0, prev = -1, time = 0)
