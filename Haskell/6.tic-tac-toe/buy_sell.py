# https://leetcode.com/problems/best-time-to-buy-and-sell-stock-ii/description/

class Solution:
    def maxProfit(self, s: List[int]) -> int:
        n = len(s)
        o = [0 for _ in range(n)]
        no = [0 for _ in range(n)]

        o[0] = -s[0]
        no[0] = 0

        for i in range(1, n):
            o[i] = max(o[i-1], no[i-1] - s[i])
            no[i] = max(no[i-1], o[i-1] + s[i])

        return no[n-1]
