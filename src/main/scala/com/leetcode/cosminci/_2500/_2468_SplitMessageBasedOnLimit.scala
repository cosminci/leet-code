package com.leetcode.cosminci._2500

object _2468_SplitMessageBasedOnLimit:

  def splitMessage(msg: String, limit: Int): Array[String] =
    def len(n: Int) = n.toString.length

    val k = Iterator
      .iterate((0, 0)) { case (totalLen, k) => (totalLen + len(k + 1), k + 1) }
      .dropWhile { case (tLen, k) => 3 + len(k) * 2 < limit && tLen + msg.length + (3 + len(k)) * k > limit * k }
      .next()._2

    if 3 + len(k) * 2 >= limit then Array.empty
    else
      (1 to k)
        .foldLeft(0, Array.empty[String]) { case ((i, result), j) =>
          val chunkSize = limit - (len(j) + 3 + len(k))
          (i + chunkSize, result :+ s"${msg.slice(i, i + chunkSize)}<$j/$k>")
        }._2
