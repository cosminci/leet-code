package com.leetcode.cosminci._1900

import scala.collection.immutable.TreeSet
import scala.util.chaining.*

object _1834_SingleThreadedCPU:

  case class Task(start: Int, time: Int, i: Int)

  def getOrder(input: Array[Array[Int]]): Array[Int] =
    val tasks   = input.zipWithIndex.map { case (Array(s, t), i) => Task(s, t, i) }.sortBy(t => (t.start, t.time, t.i))
    val pending = TreeSet.empty[Task](Ordering.by(t => (t.time, t.i)))

    Iterator
      .iterate((0, 0, pending, Array.empty[Int])) { case (i, time, pending, res) =>
        val (j, newPending) = Iterator
          .iterate((i, pending)) { case (i, pending) => (i + 1, pending + tasks(i)) }
          .dropWhile { case (i, pending) => i < tasks.length && tasks(i).start <= time }.next()

        newPending.headOption match
          case Some(task) => (j, time + task.time, newPending.tail, res :+ task.i)
          case None => if j < tasks.length then (j, tasks(j).start, newPending, res) else (j, time, newPending, res)
      }
      .dropWhile { case (_, _, _, res) => res.length != tasks.length }.next()
      .pipe { case (_, _, _, res) => res }
