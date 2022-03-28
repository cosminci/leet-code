package io.github.cosminci.leetcode._100

object _81_SearchInRotatedSortedArrayII:
  def main(args: Array[String]): Unit =
    println(search(Array(2, 5, 6, 0, 0, 1, 2), 2))
    println(search(Array(2, 5), 2))

  def search(nums: Array[Int], target: Int): Boolean =
    @annotation.tailrec
    def dfs(l: Int, r: Int): Boolean =
      if l > r then false
      else
        val mid = l + (r - l) / 2
        if nums(mid) == target then true
        else if nums(l) == nums(mid) && nums(mid) == nums(r) then dfs(l + 1, r - 1)
        else if nums(l) <= nums(mid) then
          if nums(l) <= target && nums(mid) > target then dfs(l, r = mid - 1)
          else dfs(l = mid + 1, r)
        else if nums(mid) < target && nums(r) >= target then dfs(l = mid + 1, r)
        else dfs(l, r = mid - 1)

    dfs(l = 0, r = nums.length - 1)
