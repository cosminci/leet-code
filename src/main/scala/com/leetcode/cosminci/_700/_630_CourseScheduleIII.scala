package com.leetcode.cosminci._700

import scala.collection.mutable

object _630_CourseScheduleIII:

  def main(args: Array[String]): Unit =
    val courses = Array(Array(100, 200), Array(200, 1300), Array(1000, 1250), Array(2000, 3200))
    println(scheduleCourseTopDown(courses))
    println(scheduleCoursePQueue(courses))

  def scheduleCourseTopDown(courses: Array[Array[Int]]): Int =
    courses.sortInPlaceBy(_.last)
    val mem = mutable.Map.empty[(Int, Int), Int]

    def dfs(idx: Int, currentTime: Int): Int = mem.getOrElseUpdate((idx, currentTime),
      if idx == courses.length then 0
      else {
        val Array(duration, endLimit) = courses(idx)
        if currentTime + duration <= endLimit then
          (1 + dfs(idx + 1, currentTime + duration)).max(dfs(idx + 1, currentTime))
        else dfs(idx + 1, currentTime)
      })

    dfs(idx = 0, currentTime = 0)

  def scheduleCoursePQueue(courses: Array[Array[Int]]): Int =
    courses.sortInPlaceBy(_.last)

    var currentTime = 0
    val taken =
      given Ordering[Array[Int]] = (x, y) => x.head.compare(y.head)
      mutable.PriorityQueue.empty[Array[Int]]

    courses.foreach { case course @ Array(duration, endLimit) =>
      if currentTime + duration <= endLimit then
        taken.enqueue(course)
        currentTime += duration
      else if taken.headOption.exists(_.head > course.head) then
        val removed = taken.dequeue()
        taken.enqueue(course)
        currentTime += (course.head - removed.head)
    }

    taken.length
