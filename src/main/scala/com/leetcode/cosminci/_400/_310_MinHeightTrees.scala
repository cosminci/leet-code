package com.leetcode.cosminci._400

import scala.collection.mutable

object _310_MinHeightTrees:
  def main(args: Array[String]): Unit =
    println(findMinHeightTrees(4, Array(Array(1, 0), Array(1, 2), Array(1, 3))))
    println(findMinHeightTrees(6, Array(Array(3, 0), Array(3, 1), Array(3, 2), Array(3, 4), Array(5, 4))))
    println(findMinHeightTrees(2, Array(Array(0, 1))))
    println(findMinHeightTrees(1, Array.empty))

  def findMinHeightTrees(n: Int, edges: Array[Array[Int]]): List[Int] =
    val adjacencyList = mutable.Map.from((0 until n).map(_ -> mutable.Set.empty[Int]))
    edges.foreach { case Array(n1, n2) =>
      adjacencyList(n1).add(n2)
      adjacencyList(n2).add(n1)
    }

    while adjacencyList.size > 2 do
      val leaves = adjacencyList.filter(_._2.size == 1)
      leaves.foreach { case (leaf, neighbours) =>
        neighbours.foreach(n => adjacencyList(n).remove(leaf))
        adjacencyList.remove(leaf)
      }

    adjacencyList.keys.toList
