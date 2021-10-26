package io.github.cosminci.leetcode._1700

object _1629_SlowestKey:
  def main(args: Array[String]): Unit =
    println(slowestKey(Array(9, 29, 49, 50), "cbcd"))

  private def slowestKey(releaseTimes: Array[Int], keysPressed: String): Char =
    val durations = releaseTimes.indices.map { i =>
      releaseTimes(i) - (if i > 0 then releaseTimes(i - 1) else 0)
    }
    val max = durations.max
    durations.zip(keysPressed).collect { case (duration, key) if duration == max => key }.max
