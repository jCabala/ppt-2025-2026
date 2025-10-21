
# Top-down approach with memoization
def fib_td(n):
    memo = {0:1, 1: 1}
    def fib(n):
        if n not in memo:
            memo[n] = fib(n-1) + fib(n-2)

        return memo[n]
    
    return fib(n)

# Bottom-up approach with tabulation
def fib_bu(n):
   if n == 0:
       return 0
   if n == 1:
       return 1
   
   fibs = [0 for _ in range(n+1)]
   fibs[1] = 1

   for i in range(2, n+1):
       fibs[i] = fibs[i-1] + fibs[i-2]

   return fibs[n]

# Optimized space approach