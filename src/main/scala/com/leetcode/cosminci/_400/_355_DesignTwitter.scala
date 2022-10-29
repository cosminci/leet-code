package com.leetcode.cosminci._400

import scala.collection.mutable

object _355_DesignTwitter {
  def main(args: Array[String]): Unit = {
    val twitter = new Twitter
    twitter.postTweet(1, 5)
    println(twitter.getNewsFeed(1))
    twitter.follow(1, 2)
    twitter.postTweet(2, 6)
    println(twitter.getNewsFeed(1))
    twitter.unfollow(1, 2)
    println(twitter.getNewsFeed(1))
  }

  class Twitter() {
    private val feeds = mutable.Map.empty[Int, mutable.Queue[(Int, Int)]]
    private val followees = mutable.Map.empty[Int, mutable.Set[Int]]

    var time = 0
    def postTweet(userId: Int, tweetId: Int) = {
      val userFeed = feeds.getOrElseUpdate(userId, mutable.Queue.empty)
      userFeed.enqueue((tweetId, time))
      time += 1
    }

    def getNewsFeed(userId: Int): List[Int] = {
      val ownTweets = feeds.get(userId).map(_.toList).getOrElse(List.empty)
      val followeeTweets = followees.getOrElse(userId, mutable.Set.empty).flatMap(feeds.get).flatten
      (ownTweets ++ followeeTweets).sortBy(_._2).map(_._1).takeRight(10).reverse
    }

    def follow(followerId: Int, followeeId: Int): Unit =
      followees.getOrElseUpdate(followerId, mutable.Set.empty).add(followeeId)

    def unfollow(followerId: Int, followeeId: Int): Unit =
      followees.get(followerId).foreach(_.remove(followeeId))
  }
}
