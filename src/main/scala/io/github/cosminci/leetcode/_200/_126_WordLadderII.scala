package io.github.cosminci.leetcode._200

import scala.collection.mutable

object _126_WordLadderII:

  def main(args: Array[String]): Unit =
    println(findLadders("hit", "cog", List("hot", "dot", "dog", "lot", "log", "cog")))

  def findLadders(beginWord: String, endWord: String, wordList: List[String]): List[List[String]] =
    val graph = buildGraph(beginWord +: wordList)

    val queue  = mutable.Queue(beginWord)
    val depths = mutable.Map.from(wordList.map(_ -> -1) :+ (beginWord -> 0))
    val paths  = mutable.Map(beginWord -> Seq(Seq(beginWord))).withDefaultValue(Seq.empty)

    while queue.nonEmpty do
      val w = queue.dequeue()
      if w == endWord then return paths(w).map(_.toList).toList
      graph
        .getOrElse(w, Set.empty)
        .filter(nei => depths(nei) == -1 || depths(nei) == depths(w) + 1)
        .foreach { nei =>
          if depths(nei) == -1 then
            queue.append(nei)
            depths(nei) = depths(w) + 1
          paths(w).foreach(path => paths.update(nei, paths(nei) :+ (path :+ nei)))
        }

    List.empty

  private def buildGraph(words: List[String]) =
    words
      .foldLeft(Map.empty[String, Set[String]]) { (patterns, w) =>
        w.indices.foldLeft(patterns) { (patterns, i) =>
          patterns.updated(w.updated(i, '*'), patterns.getOrElse(w.updated(i, '*'), Set.empty) + w)
        }
      }
      .values
      .foldLeft(Map.empty[String, Set[String]]) { (graph, pset) =>
        pset.toSeq.combinations(2).foldLeft(graph) { case (graph, Seq(p1, p2)) =>
          graph
            .updated(p1, graph.getOrElse(p1, Set.empty) + p2)
            .updated(p2, graph.getOrElse(p2, Set.empty) + p1)
        }
      }
