package com.leetcode.cosminci._2400

object _2363_MergeSimilarItems:

  def mergeSimilarItems(items1: Array[Array[Int]], items2: Array[Array[Int]]): List[List[Int]] =
    (items1 ++ items2)
      .groupMapReduce(_.head)(_.last)(_ + _)
      .toList
      .sorted
      .map { case (value, totalWeight) => List(value, totalWeight) }
