package io.github.cosminci.leetcode._2100

import scala.collection.mutable

object _2049_CountNodesWithHighestScore:
  def main(args: Array[String]): Unit =
    println(countHighestScoreNodes(Array(-1, 2, 0, 2, 0)))

  def countHighestScoreNodes(parents: Array[Int]): Int =
    val tree = parents.zipWithIndex.foldLeft(Map.empty[Int, Seq[Int]]) {
      case (tree, (parent, child)) =>
        tree.updated(parent, tree.getOrElse(parent, Seq.empty) :+ child)
    }

    val freq = mutable.Map.empty[Long, Int]
    def dfs(node: Int): Long =
      val (product, sum) = tree.getOrElse(node, Seq.empty).foldLeft(1L, 0L) {
        case ((p, s), child) =>
          val childScore = dfs(child)
          (p * childScore, s + childScore)
      }
      val score = product * (parents.length - 1 - sum).max(1)
      freq.update(score, freq.getOrElse(score, 0) + 1)
      sum + 1

    dfs(node = 0)
    freq.max._2
