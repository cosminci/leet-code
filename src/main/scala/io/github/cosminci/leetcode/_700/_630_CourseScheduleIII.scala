package io.github.cosminci.leetcode._700

import scala.collection.mutable

object _630_CourseScheduleIII:

  def main(args: Array[String]): Unit =
    val courses = Array(Array(100, 200), Array(200, 1300), Array(1000, 1250), Array(2000, 3200))
    println(scheduleCourseTopDown(courses))
    println(scheduleCoursePQueue(courses))

  def scheduleCourseTopDown(courses: Array[Array[Int]]): Int =
    courses.sortInPlaceBy(_.last) // sort by end limit

    val mem = mutable.Map.empty[(Int, Int), Int]

    def dfs(idx: Int, currentTime: Int): Int =
      if idx == courses.length then return 0
      if mem.contains((idx, currentTime)) then return mem((idx, currentTime))

      val Array(duration, endLimit) = courses(idx)
      val result =
        if currentTime + duration <= endLimit then
          math.max(1 + dfs(idx + 1, currentTime + duration), dfs(idx + 1, currentTime))
        else dfs(idx + 1, currentTime)

      mem.update((idx, currentTime), result)
      result

    dfs(idx = 0, currentTime = 0)

  def scheduleCoursePQueue(courses: Array[Array[Int]]): Int =
    courses.sortInPlaceBy(_.last) // sort by end limit

    var currentTime = 0
    val taken =
      given Ordering[Array[Int]] = (x, y) => x.head.compare(y.head) // max heap by duration
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
