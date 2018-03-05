# Uses python3
import sys


def find_pisano_period(m):
    previous = 0
    current = 1


    sum_L = [None] * (m*m)
    sum_L[0] = 0
    sum_L[1] = 1

    for i in range(m*m):
        previous, current = current, (previous + current) % m

        if previous == 0 and current == 1:
            return i+1, sum_L[:i+2]

        sum_L[i+2] = (sum_L[i+1] + current) % 10

def fibonacci_sum(n):
    m=10
    pisano_period, sum_L = find_pisano_period(m)
    # print (pisano_period)
    # print (sum_L)

    Q = n // pisano_period
    R = n % pisano_period

    # print (Q)
    # print (R)

    return int((sum_L[pisano_period] * Q + sum_L[R]) % 10 )



def fibonacci_sum_naive(n):
    if n <= 1:
        return n

    previous = 0
    current  = 1
    sum      = 1

    for _ in range(n - 1):
        previous, current = (current) % 10, (previous + current) % 10
        sum += current
        sum %= 10

    return sum % 10

if __name__ == '__main__':
    input = sys.stdin.read()
    n = int(input)
    print(fibonacci_sum(n))
