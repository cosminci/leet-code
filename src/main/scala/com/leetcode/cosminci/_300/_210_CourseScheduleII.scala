package com.leetcode.cosminci._300

object _210_CourseScheduleII:

  def main(args: Array[String]): Unit =
    println(findOrder(1, Array.empty).toList)

  def findOrder(numCourses: Int, prerequisites: Array[Array[Int]]): Array[Int] =
    val adjList = (0 until numCourses).map(_ -> Set.empty[Int]).toMap ++
      prerequisites.groupMap(_.head)(_.last).mapValues(_.toSet).toMap

    @annotation.tailrec
    def traverse(adjList: Map[Int, Set[Int]], acc: Array[Int]): Array[Int] =
      lazy val (roots, nonRoots) = adjList.partition(_._2.isEmpty)
      if adjList.isEmpty then acc
      else if roots.isEmpty then Array.empty
      else traverse(nonRoots.mapValues(_ -- roots.keys).toMap, acc ++ roots.keys)

    traverse(adjList, Array.empty)
