# https://leetcode.com/problems/climbing-stairs/?envType=problem-list-v2&envId=dynamic-programming

class Solution(object):
    def climbStairs(self, n):
        """
        :type n: int
        :rtype: int
        """
        value = [0 for _ in range(n + 1)] 
        value[0] = 1
        value[1] = 2

        for i in range(2, n):
            value[i] = value[i-1] + value[i-2]


        return value[n-1]

        