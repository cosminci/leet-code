package com.leetcode.cosminci._600

import scala.collection.immutable.TreeSet
import scala.util.chaining.*

object _502_IPO:

  def findMaximizedCapital(k: Int, w: Int, profits: Array[Int], capital: Array[Int]): Int =
    // x - project index to ensure uniqueness inside TreeSet, alternative is to use mutable PQueue
    val projects = capital.zip(profits).sorted.zipWithIndex.map { case ((c, p), x) => (c, p, x) }
    (0 until k)
      .foldLeft(w, 0, TreeSet.empty[(Int, Int)]) { case ((w, i, av), _) =>
        Iterator
          .iterate((i, av)) { case (i, av) => (i + 1, av + projects(i).pipe { case (_, p, x) => (p, x) }) }
          .dropWhile { case (i, _) => i < projects.length && projects(i).pipe { case (c, _, _) => c <= w } }
          .next()
          .pipe { case (i, av) =>
            av.lastOption match
              case None         => return w
              case Some((p, _)) => (w + p, i, av.dropRight(1))
          }
      }
      .pipe { case (w, _, _) => w }
