package io.github.cosminci.leetcode._1000

object _950_RectangleAreaII:
  def rectangleArea(rectangles: Array[Array[Int]]): Int =
    val n = rectangles.length
    val (xSet, ySet) = rectangles.foldLeft(Set.empty[Int], Set.empty[Int]) {
      case ((xAcc, yAcc), Array(x1, y1, x2, y2)) =>
        (xAcc + x1 + x2, yAcc + y1 + y2)
    }
    val (xSeq, ySeq) = (xSet.toSeq.sorted, ySet.toSeq.sorted)
    val xMap = xSeq.indices.foldLeft(Map.empty[Int, Int]) { case (xAcc, i) =>
      xAcc.updated(xSeq(i), i)
    }
    val yMap = ySeq.indices.foldLeft(Map.empty[Int, Int]) { case (yAcc, i) =>
      yAcc.updated(ySeq(i), i)
    }

    val grid = Array.ofDim[Boolean](xSeq.length, ySeq.length)
    rectangles.foreach { case Array(x1, y1, x2, y2) =>
      (xMap(x1) until xMap(x2)).foreach { x =>
        (yMap(y1) until yMap(y2)).foreach { y =>
          grid(x)(y) = true
        }
      }
    }
    var area = 0L
    grid.indices.foreach { i =>
      grid(i).indices.foreach { j =>
        if grid(i)(j) then area += (xSeq(i + 1) - xSeq(i)).toLong * (ySeq(j + 1) - ySeq(j))
      }
    }

    (area % 1_000_000_007).toInt
