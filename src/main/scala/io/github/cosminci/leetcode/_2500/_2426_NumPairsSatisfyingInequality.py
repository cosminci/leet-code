from sortedcontainers import SortedList


class Solution:
    def numberOfPairs(self, nums1: List[int], nums2: List[int], diff: int) -> int:
        sl, ans = SortedList(), 0
        for a, b in zip(nums1, nums2):
            ans += sl.bisect_right(a - b + diff)
            sl.add(a - b)
        return ans
