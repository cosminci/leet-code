package io.github.cosminci.leetcode._1100

import scala.collection.mutable

object _1024_VideoStitching:
  def main(args: Array[String]): Unit =
    println(videoStitching(Array(Array(0, 2), Array(4, 6), Array(8, 10), Array(1, 9), Array(1, 5), Array(5, 9)), 10))

  def videoStitching(clips: Array[Array[Int]], time: Int): Int =
    val clipsLeft  = mutable.Queue.from(clips.sortBy(_.head))
    val clipsAdded = mutable.Stack.empty[Array[Int]]

    while clipsAdded.headOption.forall(_.last < time) do
      val currentEnd = clipsAdded.headOption.map(_.last).getOrElse(0)
      clipsLeft.dequeueWhile(_.head <= currentEnd).maxByOption(_.last) match
        case None =>
          return -1
        case Some(clip) =>
          clipsAdded.push(clip)

    clipsAdded.length
