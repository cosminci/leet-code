package com.leetcode.cosminci.misc

import scala.collection.mutable

object OnCallSchedules {
  case class Schedule(start: Int, end: Int, eng: String)
  case class Rotation(start: Int, end: Int, engs: Seq[String])

  sealed trait Shift {
    def ts: Int
    def eng: String
  }
  case class Start(ts: Int, eng: String) extends Shift
  case class End(ts: Int, eng: String) extends Shift

  def rotations(schedules: Seq[Schedule]): Seq[Rotation] =
    schedules.flatMap {
      case Schedule(start, end, eng) =>
        Seq(Start(start, eng), End(end, eng))
    }.groupBy(_.ts).toSeq.sortBy(_._1)
      .foldLeft(Option.empty[Int], Seq.empty[Rotation], Set.empty[String]) {
        case ((maybePrevTs, results, onCall), (ts, shifts)) =>
          val singleDayShifts = shifts.groupBy(_.eng).collect {
            case (eng, shifts) if shifts.size == 2 => eng
          }.toSeq
          val singleDayRotation = Option.when(singleDayShifts.nonEmpty)(Rotation(ts, ts, singleDayShifts))

          val newOnCall = onCall --
            shifts.collect { case End(_, eng) if !singleDayShifts.contains(eng) => eng } ++
            shifts.collect { case Start(_, eng) if singleDayShifts.contains(eng) => eng}

          maybePrevTs match {
            case None => (Some(ts), results ++ singleDayRotation, newOnCall)
            case Some(prevTs) => (Some(ts), results ++ singleDayRotation :+ Rotation(prevTs, ts, onCall.toSeq), newOnCall)
          }
    }._2
}
