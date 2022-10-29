package com.leetcode.cosminci._900

import scala.collection.mutable

object _855_ExamRoom {

  def main(args: Array[String]): Unit = {
    val room = new ExamRoom(10)
    println(room.seat())
    println(room.seat())
    println(room.seat())
    println(room.seat())
    room.leave(4)
    println(room.seat())
  }

  class ExamRoom(n: Int) {
    private val seated = mutable.ListBuffer.empty[Int]

    def seat(): Int = {
      val seat =
        if (seated.length == 0) 0
        else
          Seq((seated.head, 0), (n - 1 - seated.last, n - 1))
            .concat(seated.zip(seated.tail)
              .maxByOption { case (seatA, seatB) => (seatB - seatA) / 2 }
              .map { case (seatA, seatB) => ((seatB - seatA) / 2, (seatB + seatA) / 2) })
            .maxBy { case (distance, position) => (distance, -position) }
            ._2

      seated.insert(seated.search(seat).insertionPoint, seat)
      seat
    }

    def leave(p: Int): Unit = seated.remove(seated.indexOf(p))
  }
}
