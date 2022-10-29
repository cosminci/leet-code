package com.leetcode.cosminci._500

import java.util.LinkedList

object _406_QueueReconstructionByHeight:

  def reconstructQueue(people: Array[Array[Int]]): Array[Array[Int]] =
    people
      .sortBy { case Array(h, k) => (-h, k) }
      .foldLeft(Array.empty[Array[Int]]) {
        case (result, p @ Array(_, k)) =>
          val (fh, sh) = result.splitAt(k)
          (fh :+ p) ++ sh
      }
