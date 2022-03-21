package io.github.cosminci.leetcode._800

object _763_PartitionLabels:

  def main(args: Array[String]): Unit =
    println(partitionLabels("ababcbacadefegdehijhklij"))

  def partitionLabels(s: String): List[Int] =
    val ends = s.indices.foldLeft(Map.empty[Char, Int])((ends, i) => ends + (s(i) -> i))

    s.indices
      .foldLeft(Seq.empty[Int], 0, 0) { case ((result, start, last), i) =>
        val newLast = last.max(ends(s(i)))
        val (newResult, newStart) =
          if newLast == i then (result :+ (newLast - start + 1), newLast + 1)
          else (result, start)

        (newResult, newStart, newLast)
      }._1.toList
