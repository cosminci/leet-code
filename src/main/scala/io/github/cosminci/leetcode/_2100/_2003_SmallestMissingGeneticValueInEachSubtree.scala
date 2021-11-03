package io.github.cosminci.leetcode._2100

object _2003_SmallestMissingGeneticValueInEachSubtree:
  def main(args: Array[String]): Unit =
    print(smallestMissingValueSubtree(Array(-1, 0, 1, 0, 3, 3), Array(5, 4, 6, 2, 1, 3)).toSeq)

  def smallestMissingValueSubtree(parents: Array[Int], nums: Array[Int]): Array[Int] =
    val n = parents.length

    val result           = Array.fill(n)(1)
    val nodeWithValueOne = nums.indexOf(1)
    if nodeWithValueOne == -1 then return result

    val children = (1 until n).foldLeft(Map.empty[Int, Seq[Int]].withDefaultValue(Seq.empty)) { case (acc, i) =>
      acc.updated(parents(i), acc(parents(i)).appended(i))
    }

    val seen = Array.ofDim[Int](n + 2)
    def dfs(node: Int): Unit =
      if seen(nums(node)) == 0 then
        children(node).foreach(dfs)
        seen(nums(node)) = 1

    var (curr, minMissing) = (nodeWithValueOne, 1)
    while curr >= 0 do
      dfs(curr)
      while seen(minMissing) != 0 do minMissing += 1
      result(curr) = minMissing
      curr = parents(curr)

    result
