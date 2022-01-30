package io.github.cosminci.leetcode._2200

import scala.collection.mutable

import io.github.cosminci.utils.UnionFind

object _2157_GroupsOfStrings {

  def groupStrings(words: Array[String]): Array[Int] = {
    val indices = words.indices
    val masks = indices.map(i => bitmask(words(i)))

    val maskToIdx = masks
      .zipWithIndex
      .flatMap { case (mask, i) => replaceMasks(mask).map(m => (m, i)) }
      .toMap

    val uf = new UnionFind[Int]

    masks.indices.foreach { i =>
      (0 until 26).foreach { bitIdx =>
        val connectedMasks =
          if ((masks(i) >> bitIdx & 1) == 0) Array(masks(i) + (1 << bitIdx))
          else Array(masks(i) - (1 << bitIdx), masks(i) - (1 << bitIdx) + (1 << 26))

        connectedMasks.foreach(maskToIdx.get(_).foreach(j => uf.union(i, j)))
      }
    }

    Array(indices.map(uf.find).toSet.size, indices.map(uf.rank).max)
  }

  private def bitmask(w: String): Int = w.foldLeft(0)((bits, c) => bits | 1 << c - 'a')

  private def replaceMasks(mask: Int) =
    mask +: (0 until 26)
      .filter(bitIdx => (mask >> bitIdx & 1) == 1)
      .map(bitIdx => mask - (1 << bitIdx) + (1 << 26)) // use 27th bit as * wildcard for replace op
}
