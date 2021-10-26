package io.github.cosminci.leetcode._1700

object _1653_MinDeletionsToMakeStringBalanced:
  def main(args: Array[String]): Unit =
    println(minimumDeletions("a"))
    println(minimumDeletions("aababbab"))
    println(minimumDeletions("bbaaaaabb"))

  private def minimumDeletions(s: String): Int =
    val (aCount, bCount) = (s.count(_ == 'a'), s.count(_ == 'b'))

    var (aAccumulated, bAccumulated) = (0, 0)
    var minDeletions                 = s.length

    s.indices.foreach { i =>
      minDeletions = math.min(minDeletions, bAccumulated + aCount - aAccumulated)
      if s(i) == 'a' then aAccumulated += 1 else bAccumulated += 1
    }

    math.min(minDeletions, bAccumulated + aCount - aAccumulated)
