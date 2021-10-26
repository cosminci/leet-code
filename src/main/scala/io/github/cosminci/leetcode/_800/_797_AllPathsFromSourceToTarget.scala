package io.github.cosminci.leetcode._800

import scala.collection.mutable

object _797_AllPathsFromSourceToTarget:
  def main(args: Array[String]): Unit =
    println(
      allPathsSourceTarget(
        Array(Array(4, 3, 1), Array(3, 2, 4), Array(3), Array(4), Array.empty[Int])
      ).map(_.toSeq).toSeq
    )

  private def allPathsSourceTarget(graph: Array[Array[Int]]): List[List[Int]] =
    val target = graph.length - 1

    def dfs(node: Int, path: List[Int]): List[List[Int]] =
      if node == target then List(path :+ target)
      else graph(node).toList.flatMap(next => dfs(next, path :+ node))

    dfs(node = 0, path = List.empty)
