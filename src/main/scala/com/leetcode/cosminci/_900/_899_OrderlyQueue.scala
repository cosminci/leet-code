package com.leetcode.cosminci._900

object _899_OrderlyQueue:
  def main(args: Array[String]): Unit =
    println(orderlyQueue("dbca", 1))

  def orderlyQueue(s: String, k: Int): String =
    def rotation(i: Int): String =
      val (fh, sh) = s.splitAt(i)
      s"$sh$fh"

    if k > 1 then s.sorted else rotation(s.indices.minBy(rotation))
