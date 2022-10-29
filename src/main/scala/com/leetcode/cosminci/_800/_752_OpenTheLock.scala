package com.leetcode.cosminci._800

import scala.collection.mutable

object _752_OpenTheLock:
  def main(args: Array[String]): Unit =
    println(openLock(Array("0201", "0101", "0102", "1212", "2002"), "0202"))
    println(openLock(Array("8888"), "0009"))
    println(openLock(Array("8887", "8889", "8878", "8898", "8788", "8988", "7888", "9888"), "8888"))
    println(openLock(Array("0000"), "8888"))

  def openLock(deadends: Array[String], target: String): Int =
    if deadends.contains("0000") then return -1

    val targetDigits = target.toCharArray.toSeq.map(_ - '0')
    val root         = Seq(0, 0, 0, 0)
    val visited      = mutable.Set.from(deadends.toSeq.map(s => s.toCharArray.toSeq.map(_ - '0')) :+ root)
    val toVisit      = mutable.Queue((root, 0))

    while toVisit.nonEmpty do
      val (curr, steps) = toVisit.dequeue()
      if curr == targetDigits then return steps

      curr.indices
        .flatMap { i =>
          Seq(
            curr.updated(i, math.floorMod(curr(i) + 1, 10)),
            curr.updated(i, math.floorMod(curr(i) - 1, 10))
          )
        }
        .foreach { next =>
          if !visited.contains(next) then
            visited.add(next)
            toVisit.enqueue((next, steps + 1))
        }
    -1
