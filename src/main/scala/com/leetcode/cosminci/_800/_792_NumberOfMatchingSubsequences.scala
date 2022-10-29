package com.leetcode.cosminci._800

import scala.collection.mutable

object _792_NumberOfMatchingSubsequences:
  def main(args: Array[String]): Unit =
    println(numMatchingSubseq("abcde", Array("a", "bb", "acd", "ace")))

  def numMatchingSubseq(s: String, words: Array[String]): Int =
    s.foldLeft(words.toSeq.map(_.iterator).groupBy(_.next()), 0) {
      case ((iterators, count), char) =>
        val toAdvance = iterators.getOrElse(char, Seq.empty)
        toAdvance.foldLeft(iterators.removed(char), count) {
          case ((iterators, count), it) =>
            if it.hasNext then
              (iterators.updatedWith(it.next()) {
                  case None                 => Some(Seq(it))
                  case Some(otherIterators) => Some(otherIterators :+ it)
                }, count)
            else (iterators, count + 1)
        }
    }._2
