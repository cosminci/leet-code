package io.github.cosminci.leetcode._300

import scala.collection.mutable

object _210_CourseScheduleII:

  def main(args: Array[String]): Unit =
    println(findOrder(1, Array.empty).toList)

  def findOrder(numCourses: Int, prerequisites: Array[Array[Int]]): Array[Int] =
    val adjacencyList = mutable.Map.empty[Int, mutable.Set[Int]]
    (0 until numCourses).foreach(c => adjacencyList.update(c, mutable.Set.empty))
    prerequisites.foreach { case Array(c, dep) =>
      adjacencyList.update(c, adjacencyList(c).addOne(dep))
    }

    val completable      = Array.ofDim[Int](numCourses)
    val completableOrder = mutable.ListBuffer.empty[Int]
    val visited          = mutable.Set.empty[Int]

    def dfs(course: Int): Boolean =
      if completable(course) == 1 then return true
      if visited.contains(course) then return false
      visited.add(course)
      if adjacencyList(course).isEmpty || adjacencyList(course).forall(dfs) then
        completable(course) = 1
        completableOrder.addOne(course)
        true
      else false

    if (0 until numCourses).forall(dfs) then completableOrder.toArray else Array.empty
