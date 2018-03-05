# Uses python3
def calc_fib(n):
    if (n <= 1):
        return n

    if L[n-1] is not None:
        n_1 = L[n-1]
    else:
        L[n-1] = calc_fib(n - 1)
        n_1 = L[n-1]
    
    if L[n-2] is not None:
        n_2 = L[n-2]
    else:
        L[n-2] = calc_fib(n - 2)
        n_2 = L[n-2]
    
    
    return n_1 + n_2
    # return calc_fib(n - 1) + calc_fib(n - 2)

n = int(input())

L = [None]*n

print(calc_fib(n))
