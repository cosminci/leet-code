package com.leetcode.cosminci._2600

import scala.util.chaining.*

object _2564_SubstringXorQueries:

  def substringXorQueries(s: String, queries: Array[Array[Int]]): Array[Array[Int]] =
    val xors = (0 until s.length).foldLeft(Map.empty[Int, Array[Int]]) { (xors, i) =>
      if s(i) == '0' then
        if !xors.contains(0) then xors.updated(0, Array(i, i))
        else xors
      else
        (0 until 32)
          .filter(_ + i < s.length)
          .foldLeft(xors, 0) { case ((xors, xor), j) =>
            val newXor = (xor << 1) + Option.when(s(i + j) == '1')(1).getOrElse(0)
            if !xors.contains(newXor) then (xors.updated(newXor, Array(i, i + j)), newXor)
            else (xors, newXor)
          }
          .pipe { case (xors, _) => xors }
    }
    queries.map { case Array(l, r) => xors.getOrElse(l ^ r, Array(-1, -1)) }
