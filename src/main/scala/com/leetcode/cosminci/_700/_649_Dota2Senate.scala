package com.leetcode.cosminci._700

import scala.util.chaining.*

object _649_Dota2Senate:

  def predictPartyVictory(senate: String): String =
    val (radiant, dire) = senate.indices.partition(i => senate(i) == 'R')
    Iterator
      .iterate((radiant, dire, senate.length)) { case (r, d, j) =>
        if r.head < d.head then (r.tail :+ j, d.tail, j + 1)
        else (r.tail, d.tail :+ j, j + 1)
      }
      .dropWhile { case (r, d, _) => r.nonEmpty && d.nonEmpty }.next()
      .pipe { case (r, d, _) => if r.nonEmpty then "Radiant" else "Dire" }
