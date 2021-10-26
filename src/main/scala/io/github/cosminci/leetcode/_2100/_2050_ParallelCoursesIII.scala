package io.github.cosminci.leetcode._2100

import scala.collection.mutable

object _2050_ParallelCoursesIII:

  def main(args: Array[String]): Unit =
    println(
      minimumTime(5, Array(Array(1, 5), Array(2, 5), Array(3, 5), Array(3, 4), Array(4, 5)), Array(1, 2, 3, 4, 5))
    )

  private def minimumTime(n: Int, relations: Array[Array[Int]], time: Array[Int]): Int =
    val graph = Array.fill(n + 1)(mutable.ListBuffer.empty[Int])
    relations.foreach { case Array(prev, curr) => graph(curr).append(prev) }

    val mem = mutable.Map.empty[Int, Int]
    def dfs(course: Int): Int =
      mem.getOrElseUpdate(course, time(course - 1) + graph(course).map(dfs).maxOption.getOrElse(0))

    (1 to n).map(dfs).max
