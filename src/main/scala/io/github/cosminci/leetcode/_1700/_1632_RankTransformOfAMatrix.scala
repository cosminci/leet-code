package io.github.cosminci.leetcode._1700

import scala.collection.mutable

object _1632_RankTransformOfAMatrix:
  def main(args: Array[String]): Unit =
    println(
      matrixRankTransform(
        Array(
          Array(-24, -9, -14, -15, 44, 31, -46, 5, 20, -5, 34),
          Array(9, -40, -49, -50, 17, 40, 35, 30, -39, 36, -49),
          Array(-18, -43, -40, -5, -30, 9, -28, -41, -6, -47, 12),
          Array(11, 42, -23, 20, 35, 34, -39, -16, 27, 34, -15),
          Array(32, 27, -30, 29, -48, 15, -50, -47, -28, -21, 38),
          Array(45, 48, -1, -18, 9, -4, -13, 10, 9, 8, -41),
          Array(-42, -35, 20, -17, 10, 5, 36, 47, 6, 1, 8),
          Array(3, -50, -23, 16, 31, 2, -39, 36, -25, -30, 37),
          Array(-48, -41, 18, -31, -48, -1, -42, -3, -8, -29, -2),
          Array(17, 0, 31, -30, -43, -20, -37, -6, -43, 8, 19),
          Array(42, 25, 32, 27, -2, 45, 12, -9, 34, 17, 32)
        )
      ).map(_.toList).toList
    )

  def matrixRankTransform(matrix: Array[Array[Int]]): Array[Array[Int]] =
    val rowRanks = Array.ofDim[Int](matrix.length)
    val colRanks = Array.ofDim[Int](matrix.head.length)
    val result   = Array.ofDim[Int](matrix.length, matrix.head.length)

    val cells = matrix.indices
      .flatMap(r => matrix(r).indices.map(c => (r, c)))
      .sortBy { case (r, c) => matrix(r)(c) }

    val remainingCells = mutable.Queue.from(cells)
    while remainingCells.nonEmpty do
      var headValue      = matrix(remainingCells.head._1)(remainingCells.head._2)
      val sameValueCells = remainingCells.dequeueWhile { case (r, c) => matrix(r)(c) == headValue }

      val cellSets = disjointSets(sameValueCells.toSeq)
      cellSets.foreach { set =>
        val rows = set.map(_._1).distinct
        val cols = set.map(_._2).distinct

        val prevRowRanks = rows.map(rowRanks)
        val prevColRanks = cols.map(colRanks)
        val newRank      = (prevRowRanks ++ prevColRanks).max + 1
        set.foreach { case (r, c) => result(r)(c) = newRank }

        rows.foreach { r => rowRanks(r) = newRank }
        cols.foreach { c => colRanks(c) = newRank }
      }

    result

  private def disjointSets(cells: Seq[(Int, Int)]): Seq[Seq[(Int, Int)]] =
    var newSetId = 0
    val sets     = mutable.Map.empty[Int, Seq[(Int, Int)]]
    val colToSet = mutable.Map.empty[Int, Int]
    val rowToSet = mutable.Map.empty[Int, Int]
    cells.foreach { case (x, y) =>
      (colToSet.get(y), rowToSet.get(x)) match
        case (None, None) =>
          colToSet.update(y, newSetId)
          rowToSet.update(x, newSetId)
          sets.update(newSetId, Seq((x, y)))
          newSetId += 1
        case (Some(setId), None) =>
          rowToSet.update(x, setId)
          sets.update(setId, sets(setId).appended((x, y)))
        case (None, Some(setId)) =>
          colToSet.update(y, setId)
          sets.update(setId, sets(setId).appended((x, y)))
        case (Some(setId1), Some(setId2)) =>
          if setId1 == setId2 then sets.update(setId1, sets(setId1).appended((x, y)))
          else
            val (smaller, larger) =
              if sets(setId1).size >= sets(setId2).size then (setId2, setId1) else (setId1, setId2)
            sets(smaller).foreach { case (x, y) =>
              rowToSet.update(x, larger)
              colToSet.update(y, larger)
            }
            sets.update(larger, sets(larger) ++ sets(smaller) :+ (x, y))
            sets.remove(smaller)
    }
    sets.values.toSeq
