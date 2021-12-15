package io.github.cosminci.leetcode._2100

import scala.collection.mutable

object _2097_ValidArrangementOfPairs:
  def main(args: Array[String]): Unit =
    println(validArrangement(Array(Array(5, 1), Array(4, 5), Array(11, 9), Array(9, 4))).map(_.toSeq).toSeq)
    println(validArrangement(Array(Array(1, 3), Array(3, 2), Array(2, 1))).map(_.toSeq).toSeq)
    println(validArrangement(Array(Array(1, 2), Array(2, 1), Array(1, 3))).map(_.toSeq).toSeq)

  def validArrangement(pairs: Array[Array[Int]]): Array[Array[Int]] =
    val graph  = mutable.Map.empty[Int, mutable.Stack[Int]].withDefaultValue(mutable.Stack.empty)
    val degree = mutable.Map.empty[Int, Int].withDefaultValue(0)
    pairs.foreach { case Array(x, y) =>
      graph.getOrElseUpdate(x, mutable.Stack.empty).push(y)
      degree.update(x, degree(x) + 1)
      degree.update(y, degree(y) - 1)
    }

    val start = degree.collectFirst { case (k, v) if v == 1 => k }.getOrElse(pairs.head.head)
    val path  = mutable.ListBuffer.empty[Array[Int]]

    def dfs(curr: Int): Unit =
      while graph(curr).nonEmpty do
        val next = graph(curr).pop()
        dfs(next)
        path.prepend(Array(curr, next))

    dfs(start)
    path.toArray
