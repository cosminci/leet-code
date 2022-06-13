package io.github.cosminci.leetcode._2400

object _2301_MatchSubstringAfterReplacement:

  def matchReplacement(s: String, sub: String, mappings: Array[Array[Char]]): Boolean =
    val replacements = mappings.groupMapReduce(_.head)(_.takeRight(1))(_ concat _).view.mapValues(_.toSet).toMap

    sub.length <= s.length && s.sliding(sub.length).exists {
      _.zip(sub).forall { case (from, to) =>
        from == to || replacements.get(to).exists(_.contains(from))
      }
    }
