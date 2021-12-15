package io.github.cosminci.leetcode._2200

import scala.collection.mutable

object _2101_DetonateMaxBombs:
  def main(args: Array[String]): Unit =
    println(maximumDetonation(Array(Array(2, 1, 3), Array(6, 1, 4))))

  def maximumDetonation(bombs: Array[Array[Int]]): Int =
    def overlap(i: Int, j: Int): Boolean =
      math.pow(bombs(i)(0) - bombs(j)(0), 2) + math.pow(bombs(i)(1) - bombs(j)(1), 2) <= math.pow(bombs(i)(2), 2)

    val adjList = bombs.indices.foldLeft(Map.empty[Int, Seq[Int]].withDefaultValue(Seq.empty)) {
      case (adjList, i) =>
        bombs.indices.foldLeft(adjList) {
          case (adjList, j) =>
            if i == j || !overlap(i, j) then adjList
            else adjList.updated(i, adjList(i) :+ j)
        }
    }

    bombs.indices.map { start =>
      val visited = mutable.Set(start)
      def dfs(curr: Int): Unit =
        adjList(curr).collect {
          case next if !visited.contains(next) =>
            visited.add(next)
            dfs(next)
        }
      dfs(start)
      visited.size
    }.max
