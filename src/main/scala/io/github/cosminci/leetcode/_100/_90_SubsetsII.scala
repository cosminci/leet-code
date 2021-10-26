package io.github.cosminci.leetcode._100

object _90_SubsetsII:
  def main(args: Array[String]): Unit =
    println(subsetsWithDupRecursive(Array(4, 4, 4, 1, 4)))
    println(subsetsWithDupIterative(Array(4, 4, 4, 1, 4)))

  private def subsetsWithDupIterative(nums: Array[Int]): List[List[Int]] =
    val digitCounts = nums.groupBy(identity).view.mapValues(_.size).toMap

    digitCounts.foldLeft(List(List.empty[Int])) { case (subsets, (digit, count)) =>
      val digitCombos = (0 to count).map(c => List.fill(c)(digit))
      subsets.flatMap { set =>
        digitCombos.map(set ++ _)
      }
    }

  private def subsetsWithDupRecursive(nums: Array[Int]): List[List[Int]] =
    def dfs(idx: Int): Set[Map[Int, Int]] =
      if idx == nums.length then return Set(Map.empty)

      dfs(idx + 1).flatMap { withoutCurrent =>
        val withCurrent = withoutCurrent.updatedWith(nums(idx)) {
          case None    => Some(1)
          case Some(c) => Some(c + 1)
        }
        Set(withoutCurrent, withCurrent)
      }

    dfs(0)
      .map(counts =>
        counts.foldLeft(List.empty[Int]) { case (acc, (digit, count)) =>
          acc.prependedAll(Seq.fill(count)(digit))
        }
      )
      .toList
