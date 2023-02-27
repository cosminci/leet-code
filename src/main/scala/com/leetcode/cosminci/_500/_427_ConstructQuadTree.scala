package com.leetcode.cosminci._500

import scala.util.chaining.*

object _427_ConstructQuadTree:

  def construct(grid: Array[Array[Int]]): Node =
    if grid.length == 0 then null
    else if grid.forall(_.forall(_ == grid(0)(0))) then new Node(_value = grid(0)(0) == 1, _isLeaf = true)
    else
      val (top, bottom)             = grid.splitAt(grid.length / 2)
      val (topLeft, topRight)       = top.map(_.splitAt(grid.length / 2)).unzip
      val (bottomLeft, bottomRight) = bottom.map(_.splitAt(grid.length / 2)).unzip
      new Node(_value = true, _isLeaf = false).tap { node =>
        node.topLeft = construct(topLeft)
        node.topRight = construct(topRight)
        node.bottomLeft = construct(bottomLeft)
        node.bottomRight = construct(bottomRight)
      }

  class Node(var _value: Boolean, var _isLeaf: Boolean):
    var value: Boolean    = _value
    var isLeaf: Boolean   = _isLeaf
    var topLeft: Node     = null
    var topRight: Node    = null
    var bottomLeft: Node  = null
    var bottomRight: Node = null
