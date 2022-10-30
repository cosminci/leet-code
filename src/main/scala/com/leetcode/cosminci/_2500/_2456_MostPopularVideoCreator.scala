package com.leetcode.cosminci._2500

object _2456_MostPopularVideoCreator:

  def mostPopularCreator(creators: Array[String], ids: Array[String], views: Array[Int]): List[List[String]] =
    val videosByCreator = ids.indices
      .groupBy(creators)
      .map { case (creator, videos) => (creator, (videos.sortBy(id => (-views(id), ids(id))), videos.map(views).sum)) }

    val maxPopularity = videosByCreator.values.map { case (_, totalPopularity) => totalPopularity }.max
    videosByCreator.collect {
      case (creator, (videos, totalPopularity)) if totalPopularity == maxPopularity =>
        List(creator, ids(videos.head))
    }.toList
