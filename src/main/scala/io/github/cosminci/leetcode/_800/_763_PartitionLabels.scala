package io.github.cosminci.leetcode._800

object _763_PartitionLabels:

  def main(args: Array[String]): Unit =
    println(partitionLabels("eccbbbbdec"))
    println(partitionLabels("ababcbacadefegdehijhklij"))

  case class Interval(start: Int, end: Int)

  def partitionLabels(s: String): List[Int] =
    characterBounds(s)
      .map { case (startIdx, endIdx) => Interval(startIdx, endIdx) }
      .sortBy(_.start)
      .foldLeft(Vector.empty[Interval]) { case (mergedIntervals, newInterval) =>
        if mergedIntervals.lastOption.exists(_.end > newInterval.start) then
          val prevInterval    = mergedIntervals.last
          val updatedInterval = Interval(prevInterval.start, math.max(prevInterval.end, newInterval.end))
          mergedIntervals.dropRight(1).appended(updatedInterval)
        else mergedIntervals.appended(newInterval)
      }
      .map { interval => interval.end - interval.start + 1 }
      .toList

  private def characterBounds(s: String) =
    val startIndices = Array.fill(26)(-1)
    val endIndices   = Array.fill(26)(-1)

    s.indices.foreach { i =>
      val letterIdx = s(i) - 'a'
      if startIndices(letterIdx) == -1 then startIndices(letterIdx) = i
      endIndices(letterIdx) = i
    }

    startIndices.zip(endIndices).filter(_._1 != -1)
