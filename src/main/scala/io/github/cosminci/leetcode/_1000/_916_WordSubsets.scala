package io.github.cosminci.leetcode._1000

object _916_WordSubsets {
  def main(args: Array[String]): Unit =
    println(wordSubsets(Array("amazon","apple","facebook","google","leetcode"), Array("lo","eo")))

  def wordSubsets(words1: Array[String], words2: Array[String]): List[String] = {
    val maxCounts = words2.foldLeft(Seq.fill(26)(0))((acc, w) => acc.zip(charCounts(w)).map(_ max _))
    words1.filter(w => maxCounts.zip(charCounts(w)).forall(_ <= _)).toList
  }

  def charCounts(w: String) = w.foldLeft(Seq.fill(26)(0))((acc, c) => acc.updated(c - 'a', (acc(c - 'a') + 1)))
}
