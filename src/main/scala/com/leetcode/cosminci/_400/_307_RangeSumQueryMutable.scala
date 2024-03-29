package com.leetcode.cosminci._400

object _307_RangeSumQueryMutable:

  def main(args: Array[String]): Unit =
    val segmentTree = new NumArraySegmentTree(Array(1, 3, 5))
    val fenwickTree = new NumArrayFenwickTree(Array(1, 3, 5))
    println(segmentTree.sumRange(0, 2))
    println(fenwickTree.sumRange(0, 2))
    segmentTree.update(1, 2)
    fenwickTree.update(1, 2)
    println(segmentTree.sumRange(0, 2))
    println(fenwickTree.sumRange(0, 2))

  class NumArraySegmentTree(nums: Array[Int]):
    case class SegmentTreeNode(start: Int, end: Int, left: SegmentTreeNode, right: SegmentTreeNode, var sum: Int = 0)

    private val root = buildTree(0, nums.length - 1)
    def buildTree(start: Int, end: Int): SegmentTreeNode =
      if start > end then null
      else if start == end then SegmentTreeNode(start, end, null, null, sum = nums(start))
      else
        val mid   = start + (end - start) / 2
        val left  = buildTree(start, mid)
        val right = buildTree(mid + 1, end)
        SegmentTreeNode(start, end, left, right, left.sum + right.sum)

    def update(index: Int, value: Int): Unit =
      def dfs(node: SegmentTreeNode, i: Int, v: Int): Unit =
        if node.start == node.end then node.sum = v
        else
          val mid = node.start + (node.end - node.start) / 2
          if i <= mid then dfs(node.left, i, v)
          else dfs(node.right, i, v)
          node.sum = node.left.sum + node.right.sum

      dfs(root, index, value)

    def sumRange(left: Int, right: Int): Int =
      def dfs(node: SegmentTreeNode, l: Int, r: Int): Int =
        if node.start == l && node.end == r then node.sum
        else
          val mid = node.start + (node.end - node.start) / 2
          if r <= mid then dfs(node.left, l, r)
          else if l >= mid + 1 then dfs(node.right, l, r)
          else dfs(node.left, l, mid) + dfs(node.right, mid + 1, r)

      dfs(root, left, right)


  class NumArrayFenwickTree(nums: Array[Int]):
    private val fenwickTree =
      val prefixSum = nums.scanLeft(0)(_ + _)
      Array.tabulate(nums.length + 1)(i => prefixSum(i) - prefixSum(i - lsb(i)))

    def update(idx: Int, value: Int): Unit =
      val diff = value - nums(idx)
      nums(idx) = value
      Iterator
        .iterate(idx + 1) { i => fenwickTree(i) += diff; i + lsb(i) }
        .dropWhile(_ < fenwickTree.length)
        .next()

    def sumRange(left: Int, right: Int): Int =
      sum(right) - sum(left - 1)

    private def sum(idx: Int) =
      Iterator
        .iterate((0, idx + 1)) { case (sum, i) => (sum + fenwickTree(i), i - lsb(i))}
        .dropWhile { case (_, i) => i > 0 }
        .next()._1

    private def lsb(i: Int) = i & -i
