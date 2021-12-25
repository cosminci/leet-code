package io.github.cosminci.leetcode._2200

object _2114_MaxNumberOfWordsFoundInSentences {
  def mostWordsFound(sentences: Array[String]): Int =
    sentences.map(_.count(_ == ' ')).max + 1
}
