package com.leetcode.cosminci._300

import scala.collection.mutable

object _207_CourseSchedule:

  def main(args: Array[String]): Unit =
    println(canFinish(3, Array(Array(0, 1), Array(0, 2), Array(2, 1))))


  def canFinish(numCourses: Int, prerequisites: Array[Array[Int]]): Boolean =
    val graph = mutable.Map.empty[Int, mutable.ListBuffer[Int]]
    (0 until numCourses).foreach(c => graph.update(c, mutable.ListBuffer.empty))
    prerequisites.foreach { case Array(course, dependency) =>
      graph.update(course, graph(course).addOne(dependency))
    }

    val completable = mutable.Set.empty[Int]
    val visited     = mutable.Set.empty[Int]
    def dfs(course: Int): Boolean =
      if completable.contains(course) then return true
      if visited.contains(course) then return false
      visited.add(course)
      if graph(course).isEmpty || graph(course).forall(dfs) then
        completable.add(course)
        true
      else false

    (0 until numCourses).forall(dfs)
