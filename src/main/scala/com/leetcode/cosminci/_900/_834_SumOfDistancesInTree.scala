package com.leetcode.cosminci._900

import scala.collection.mutable

object _834_SumOfDistancesInTree:
  def main(args: Array[String]): Unit =
    println(sumOfDistancesInTree(6, Array(Array(0, 1), Array(0, 2), Array(2, 3), Array(2, 4), Array(2, 5))).toList)

  def sumOfDistancesInTree(n: Int, edges: Array[Array[Int]]): Array[Int] =
    val adjMatrix = edges.foldLeft((0 until n).map(_ -> Seq.empty[Int]).toMap) { case (prevMatrix, Array(n1, n2)) =>
      prevMatrix
        .updated(n1, prevMatrix(n1).appended(n2))
        .updated(n2, prevMatrix(n2).appended(n1))
    }

    val subtreeSizes = Array.fill[Int](n)(1)
    val distanceSums = Array.ofDim[Int](n)

    def postOrderDFS(node: Int, parent: Int): Unit =
      adjMatrix(node).foreach { child =>
        if child != parent then
          postOrderDFS(child, node)
          subtreeSizes(node) += subtreeSizes(child)
          distanceSums(node) += distanceSums(child) + subtreeSizes(child)
      }

    def preOrderDFS(node: Int, parent: Int): Unit =
      adjMatrix(node).foreach { child =>
        if child != parent then
          distanceSums(child) = distanceSums(node) - subtreeSizes(child) + (n - subtreeSizes(child))
          preOrderDFS(child, node)
      }

    postOrderDFS(0, -1)
    preOrderDFS(0, -1)

    distanceSums
