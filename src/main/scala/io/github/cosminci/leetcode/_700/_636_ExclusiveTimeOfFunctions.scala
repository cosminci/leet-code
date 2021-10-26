package io.github.cosminci.leetcode._700

import scala.collection.mutable

object _636_ExclusiveTimeOfFunctions:
  def main(args: Array[String]): Unit =
    println(exclusiveTime(2, List("0:start:0", "1:start:2", "1:end:5")))

  private def exclusiveTime(n: Int, logs: List[String]): Array[Int] =
    val callStack = mutable.Stack.empty[Int]
    val results   = Array.ofDim[Int](n)
    var prevTs    = 0

    logs.foreach { logStr =>
      val Array(idStr, op, tsStr) = logStr.split(':')
      val (id, ts)                = (idStr.toInt, tsStr.toInt)
      op match
        case "start" =>
          callStack.headOption.foreach(id => results(id) = results(id) + ts - prevTs)
          callStack.push(id)
          prevTs = ts
        case "end" =>
          val fnId = callStack.pop()
          results(fnId) = results(fnId) + ts - prevTs + 1
          prevTs = ts + 1
    }

    results
