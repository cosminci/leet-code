package com.leetcode.cosminci._300

import scala.util.chaining.*

object _207_CourseSchedule:

  def canFinish(numCourses: Int, prerequisites: Array[Array[Int]]): Boolean =
    val graph   = prerequisites.groupMap(_.head)(_.last).withDefaultValue(Array.empty[Int])
    val toVisit = (0 until numCourses).filter(graph(_).isEmpty)

    Iterator
      .iterate((toVisit, toVisit.toSet)) { case (completable, completed) =>
        val nextCompletable = (0 until numCourses)
          .filterNot(completed.contains)
          .filter(graph(_).forall(completed.contains))
        (nextCompletable, completed ++ nextCompletable)
      }
      .dropWhile { case (completable, _) => completable.nonEmpty }.next()
      .pipe { case (_, completed) => completed.size == numCourses }
