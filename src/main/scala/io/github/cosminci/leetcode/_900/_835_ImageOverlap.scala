package io.github.cosminci.leetcode._900

object _835_ImageOverlap:

  def largestOverlap(img1: Array[Array[Int]], img2: Array[Array[Int]]): Int =
    def ones(img: Array[Array[Int]]): Seq[(Int, Int)] = for
      r <- img.indices
      c <- img(r).indices
      if img(r)(c) == 1
    yield (r, c)

    val vectors = for
      (x1, y1) <- ones(img1)
      (x2, y2) <- ones(img2)
    yield (x2 - x1, y2 - y1)

    vectors
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .values
      .maxOption
      .getOrElse(0)
