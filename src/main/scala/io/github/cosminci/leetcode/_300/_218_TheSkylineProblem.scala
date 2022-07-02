package io.github.cosminci.leetcode._300

object _218_TheSkylineProblem:

  def getSkyline(buildings: Array[Array[Int]]): List[List[Int]] =
    buildings.zipWithIndex
      .flatMap { case (Array(l, r, h), idx) => Array((l, h, idx), (r, 0, idx)) }
      .sortBy { case (x, _, i) => (x, -i) }
      .foldLeft(Map.empty[Int, Int], Array(List(-1, 0))) { case ((active, skyline), (x, h, i)) =>
        val List(sx, sh) = skyline.last
        active.get(i) match
          case None =>
            val newSkyline =
              if sx == x then skyline.dropRight(1) :+ List(x, h.max(sh))
              else if sh < h then skyline :+ List(x, h)
              else skyline
            (active.updated(i, h), newSkyline)
          case Some(_) =>
            val maxRemainingH = active.removed(i).values.maxOption.getOrElse(0)
            val newSkyline =
              if sx == x then skyline.dropRight(1) :+ List(x, maxRemainingH.min(sh))
              else if sh > maxRemainingH then skyline :+ List(x, maxRemainingH)
              else skyline
            (active.removed(i), newSkyline)
      }._2.tail.toList
