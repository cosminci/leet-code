package io.github.cosminci.leetcode._2300

object _2284_SenderWithLargestWordCount:

  def largestWordCount(messages: Array[String], senders: Array[String]): String =
    messages
      .indices
      .groupMapReduce(senders(_))(messages(_).split(' ').length)(_ + _)
      .toSeq
      .maxBy { case (sender, count) => (count, sender) }
      ._1
