package io.github.cosminci.leetcode._800

object _769_MaxChunksToMakeSorted:
  def main(args: Array[String]): Unit =
    println(maxChunksToSorted(Array(2, 0, 1)))
    println(maxChunksToSorted(Array(0)))
    println(maxChunksToSorted(Array(4, 3, 2, 1, 0)))
    println(maxChunksToSorted(Array(1, 0, 2, 3, 4)))

  private def maxChunksToSorted(arr: Array[Int]): Int =
    arr.indices
      .foldLeft(0, arr(0)) { case ((chunks, currChunkEnd), i) =>
        val newChunkEnd = math.max(arr(i), currChunkEnd)
        val newChunks   = if i == newChunkEnd then chunks + 1 else chunks
        (newChunks, newChunkEnd)
      }._1
